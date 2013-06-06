package codechicken.multipart

import net.minecraft.tileentity.TileEntity
import scala.collection.immutable.Map
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import net.minecraft.world.World
import codechicken.core.vec.BlockCoord
import codechicken.multipart.handler.MultipartProxy
import cpw.mods.fml.common.FMLCommonHandler
import cpw.mods.fml.relauncher.Side
import codechicken.core.packet.PacketCustom
import net.minecraft.network.packet.Packet53BlockChange
import net.sf.cglib.reflect.FastClass
import net.sf.cglib.proxy.{ Dispatcher, Enhancer, Callback, CallbackFilter, MethodInterceptor, MethodProxy, NoOp }
import collection.mutable.{ Map => MMap }
import java.lang.reflect.Method
import org.objectweb.asm.Type.getMethodDescriptor
import Side.{ SERVER, CLIENT }
import java.util.ArrayList

object MultipartGenerator {
  private var ugenid = 0
  
  var mirror = scala.reflect.runtime.currentMirror
  var tb = mirror.mkToolBox()
  
  def getType(obj:Any) = obj.getClass
  
  object SuperSet {
    def mEquals(m1: Method, m2: Method) = {
      m1.getName == m2.getName &&
      getMethodDescriptor(m1) == getMethodDescriptor(m2)
    }
    class Filter(base: Class[_], interfaces: Array[Class[_]], val callbacks: Array[Callback])
      extends CallbackFilter{

      val methodList = new ArrayList[Method]
      Enhancer.getMethods(base, interfaces, methodList)
      val methods = methodList.toArray(Array.empty[Method])
      val methodMap = Map(methods.map{ m =>
        interfaces.zipWithIndex.find { i => i._1.getMethods.exists(mEquals(m, _))
        }.map(i => (m, i._2)).getOrElse((m, callbacks.length - 1))
      }:_*)

      def accept(method: Method) = {
        //println(s"METHOD: $method")
        //println("INDEX: " + methodMap(method))
        methodMap(method)
      }
    }

    case class InterfaceDispatcher(interface: String) extends MethodInterceptor {
      val fc = FastClass.create(Class.forName(interface))
      override def intercept(parent: Object, method: Method, args: Array[Object], proxy: MethodProxy): Object = {
        println(s"CALL: $interface, $method")
        val target = parent.asInstanceOf[TileMultipart].traitMap(interface)
        fc.getMethod(method).invoke(target, args)
      }
    }

    def apply(pIfaces: Set[String], side: Side) = MultipartGenerator.synchronized {
      val baseType = side match {
        case CLIENT => classOf[TileMultipartClient]
        case _ => classOf[TileMultipart]
      }
      val partIfaces = pIfaces.toArray.sorted
      val ifaceNames = partIfaces.map(ifaceIfaceMap(side.ordinal))
      val interfaces = ifaceNames.map(Class.forName)
      val traits = for(i <- partIfaces) yield {
        println(s"PART IFACE: $i")
        Class.forName(ifaceTraitMap(side.ordinal)(i))
      }

      val te = generatorMap.getOrElseUpdate(interfaces, {
        val s = System.currentTimeMillis
        println(s"INTERFACES: ${interfaces.mkString(", ")}")
        //val delegates = interfaces.map(_.newInstance.asInstanceOf[java.lang.Object]).toArray
        val callbacks = ifaceNames.map(getDispatcher) :+ NoOp.INSTANCE
        val enc = new Enhancer()
        enc.setSuperclass(baseType)
        enc.setInterfaces(interfaces)
        enc.setCallbackFilter(new Filter(baseType, interfaces, callbacks))
        enc.setCallbacks(callbacks)
        val dummy = baseType.cast(enc.create()).getClass
        tileIfaceMap += (dummy.getName -> ifaceNames.toSet)
        MultipartProxy.onTileClassBuilt(dummy)
        println(s"Generation [${ifaceNames.mkString(", ")}] took: ${System.currentTimeMillis - s}ms")
        enc
      }).create().asInstanceOf[TileMultipart]
      for((iface, trt) <- ifaceNames zip traits) {
        te.traitMap(iface) = trt.getConstructor(classOf[TileMultipart]).newInstance(te).asInstanceOf[TileMultipartTrait]
      }
      te
    }
    private val generatorMap = MMap[Array[Class[_]], Enhancer]()
    private val dispatcherMap = MMap[String, InterfaceDispatcher]()

    def getDispatcher(iface: String) = 
      dispatcherMap.getOrElseUpdate(iface, new InterfaceDispatcher(iface))
    
    def uniqueName(prefix: String): String = {
      val ret = prefix+"$$"+ugenid
      ugenid += 1
      return ret
    }
  }
  
  private val tileIfaceMap = MMap[String, Set[String]]() // tile class -> part ifaces (cache)
  private val ifaceIfaceMap = Array.fill(2)(MMap[String, String]()) // part interface -> trait interface
  private val ifaceTraitMap = Array.fill(2)(MMap[String, String]()) // part interface -> trait class
  private val partIfaceMap = Array.fill(2)(MMap[String, Set[String]]()) // part class -> part ifaces (cache)
  
  SuperSet(Set.empty, SERVER)//default impl, boots generator
  if(FMLCommonHandler.instance.getEffectiveSide == Side.CLIENT) // why?
    SuperSet(Set.empty, CLIENT)
  
  private def ifacesForPart(part: TMultiPart, side: Side): Set[String] =
    partIfaceMap(side.ordinal).getOrElseUpdate(part.getClass.getName,
      ifaceTraitMap(side.ordinal).keys.filter(Class.forName(_).isInstance(part)).toSet)
  
  /**
   * Check if part adds any new interfaces to tile, if so, replace tile with a new copy and call tile.addPart(part)
   * returns true if tile was replaced
   */
  private[multipart] def addPart(world:World, pos:BlockCoord, part:TMultiPart):TileMultipart = {
    val (tile, loaded) = TileMultipartObj.getOrConvertTile2(world, pos)
    val side = if(world.isRemote) CLIENT else SERVER
    val partIfaces = ifacesForPart(part, side)
    var ntile = tile
    if(tile != null) {
      if(loaded) { //perform client conversion
        world.setBlock(pos.x, pos.y, pos.z, MultipartProxy.block.blockID, 0, 1)
        world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
        PacketCustom.sendToChunk(new Packet53BlockChange(pos.x, pos.y, pos.z, world), world, pos.x>>4, pos.z>>4)

        ntile.partList(0).onConverted()
        ntile.writeAddPart(ntile.partList(0))
      }
      
      val newIfaces = partIfaces &~ tileIfaceMap(tile.getClass.getName)
      if(!newIfaces.isEmpty) {
        ntile = SuperSet(partIfaces ++ newIfaces, side)
        world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
        ntile.loadFrom(tile)
      }
    } else {
      world.setBlock(pos.x, pos.y, pos.z, MultipartProxy.block.blockID)
      ntile = SuperSet(partIfaces, side)
      world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
    }
    ntile.addPart(part)
    return ntile
  }
  
  /**
   * Check if tile satisfies all the interfaces required by parts. If not, return a new generated copy of tile
   */
  def generateCompositeTile(tile:TileEntity, parts:Seq[TMultiPart], side: Side):TileMultipart = {
    val partIfaces = parts.flatMap(ifacesForPart(_, side)).toSet
    if(tile != null && 
      tileIfaceMap.get(tile.getClass.getName) == Some(partIfaces) &&
      tile.isInstanceOf[TileMultipart]) {
      tile.asInstanceOf[TileMultipart]
    } else {
      SuperSet(partIfaces, side)
    }
  }
  
  /**
   * Check if there are any redundant interfaces on tile, if so, replace tile with new copy
   */
  def partRemoved(tile:TileMultipart, part:TMultiPart):TileMultipart = {
    val side = if(tile.worldObj.isRemote) CLIENT else SERVER
    val restIfaces = tile.partList.flatMap(ifacesForPart(_, side)).toSet
    if(!ifacesForPart(part, side).subsetOf(restIfaces)) {
      val ntile = SuperSet(restIfaces, side)
      tile.worldObj.setBlockTileEntity(tile.xCoord, tile.yCoord, tile.zCoord, ntile)
      ntile.loadFrom(tile)
      ntile
    }
    else tile
  }

  /**
   * register trt to be applied to tiles containing parts implementing pIface (and intercepting tIface methods)
   */
  def registerTrait(pIface: String, tIface: String, trt: String) {
    registerTrait(pIface, tIface, trt, CLIENT)
    registerTrait(pIface, tIface, trt, SERVER)
  }
  /**
   * register trt to be applied to tiles containing parts implementing pIface 
   * (and intercepting tIface methods) on a given effective side
   * trt (may be null) // why?
   */
  def registerTrait(pIface:String, tIface: String, trt:String, side: Side) {
    if(trt != null) {
      ifaceIfaceMap(side.ordinal)(pIface) = tIface
      ifaceTraitMap(side.ordinal)(pIface) = trt
    }
  }
}
