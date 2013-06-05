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
import net.sf.cglib.proxy.{ Dispatcher, Enhancer, CallbackHelper, ProxyRefDispatcher, NoOp }
import collection.mutable.{ Map => MMap }
import java.lang.reflect.Method
import org.objectweb.asm.Type.getMethodDescriptor

object MultipartGenerator {
  private var ugenid = 0
  
  var mirror = scala.reflect.runtime.currentMirror
  var tb = mirror.mkToolBox()
  
  def getType(obj:Any) = obj.getClass
  
  object SuperSet {
    class CallbackFilter(base: Class[_], interfaces: Array[Class[_]]) extends CallbackHelper(base, interfaces) {
      override def getCallback(method: Method): Object = {
        interfaces.find(_.getMethods.exists { m => 
          m.getName == method.getName &&
          getMethodDescriptor(m) == getMethodDescriptor(method)
        }).map { k =>
          dispatcherMap.getOrElseUpdate(k.getName, new InterfaceDispatcher(k.getName))
        }.getOrElse(NoOp.INSTANCE)
      }
    }
    case class InterfaceDispatcher(interface: String) extends ProxyRefDispatcher {
      override def loadObject(proxy: java.lang.Object): Object =
        proxy.asInstanceOf[TileMultipart].traitMap(interface)
    }
    def apply(types:Set[String], client:Boolean) = tb.synchronized {
      val baseType = if(client) classOf[TileMultipartClient] else classOf[TileMultipart]
      val sTypes = types.toArray.sorted
      val typeClasses = sTypes.map(Class.forName(_)).toArray
      val interfaces = baseType +: typeClasses

      val te = generatorMap.getOrElseUpdate(interfaces, {
        val s = System.currentTimeMillis
        println("INTERFACES: " + interfaces)
        //val delegates = interfaces.map(_.newInstance.asInstanceOf[java.lang.Object]).toArray
        val enc = new Enhancer()
        enc.setSuperclass(baseType)
        enc.setInterfaces(typeClasses)
        enc.setCallbackFilter(new CallbackFilter(baseType, typeClasses))
        val callbacks = sTypes.map{ k =>
          dispatcherMap.getOrElseUpdate(k, new InterfaceDispatcher(k))
        } :+ NoOp.INSTANCE
        enc.setCallbacks(callbacks)
        val dummy = baseType.cast(enc.create()).getClass
        tileTraitMap += (dummy.getName -> (types + baseType.getClass.getName))
        MultipartProxy.onTileClassBuilt(dummy)
        println("Generation ["+interfaces.mkString(", ")+"] took: "+(System.currentTimeMillis-s))
        enc
      }).create().asInstanceOf[TileMultipart]
      for(cls <- sTypes) {
        te.traitMap(cls) = Class.forName(cls).getConstructor(classOf[TileMultipart]).newInstance(te).asInstanceOf[TileMultipartTrait]
      }
      te
    }
    private val generatorMap = MMap[Array[Class[_]], Enhancer]()
    private val dispatcherMap = MMap[String, InterfaceDispatcher]()
    
    def uniqueName(prefix: String): String = {
      val ret = prefix+"$$"+ugenid
      ugenid += 1
      return ret
    }
  }
  
  private val tileTraitMap = MMap[String, Set[String]]()
  private val interfaceTraitMap_c = MMap[String, String]()
  private val interfaceTraitMap_s = MMap[String, String]()
  private val partTraitMap_c = MMap[String, Set[String]]()
  private val partTraitMap_s = MMap[String, Set[String]]()
  
  SuperSet(Set.empty, false)//default impl, boots generator
  if(FMLCommonHandler.instance.getEffectiveSide == Side.CLIENT)
    SuperSet(Set.empty, true)
  
  private def partTraitMap(client:Boolean) = if(client) partTraitMap_c else partTraitMap_s
  
  private def interfaceTraitMap(client:Boolean) = if(client) interfaceTraitMap_c else interfaceTraitMap_s
    
  private def traitsForPart(part:TMultiPart, client:Boolean):Set[String] =
    partTraitMap(client).getOrElseUpdate(part.getClass.getName,
      interfaceTraitMap(client).filterKeys(Class.forName(_).isInstance(part)).values.toSet)
  
  /**
   * Check if part adds any new interfaces to tile, if so, replace tile with a new copy and call tile.addPart(part)
   * returns true if tile was replaced
   */
  private[multipart] def addPart(world:World, pos:BlockCoord, part:TMultiPart):TileMultipart =
  {
    var tile = TileMultipartObj.getOrConvertTile(world, pos)
    
    var partTraits = traitsForPart(part, world.isRemote)
    var ntile = tile
    if(tile != null)
    {
      val converted = !tile.loaded
      if(converted)//perform client conversion
      {
        world.setBlock(pos.x, pos.y, pos.z, MultipartProxy.block.blockID, 0, 1)
        PacketCustom.sendToChunk(new Packet53BlockChange(pos.x, pos.y, pos.z, world), world, pos.x>>4, pos.z>>4)
        ntile.writeAddPart(ntile.partList(0))
      }
      
      val tileTraits = tileTraitMap(tile.getClass.getName)
      partTraits = partTraits.filter(!tileTraits(_))
      if(!partTraits.isEmpty)
      {
        ntile = SuperSet(partTraits++tileTraits, world.isRemote)
        world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
        ntile.loadFrom(tile)
      }
      else if(converted)
      {
        ntile.validate()
        world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
      }
      if(converted)
        ntile.partList(0).onConverted()
    }
    else
    {
      world.setBlock(pos.x, pos.y, pos.z, MultipartProxy.block.blockID)
      ntile = SuperSet(partTraits, world.isRemote)
      world.setBlockTileEntity(pos.x, pos.y, pos.z, ntile)
    }
    ntile.addPart(part)
    return ntile
  }
  
  /**
   * Check if tile satisfies all the interfaces required by parts. If not, return a new generated copy of tile
   */
  def generateCompositeTile(tile:TileEntity, parts:Seq[TMultiPart], client:Boolean):TileMultipart = 
  {
    var partTraits = parts.flatMap(traitsForPart(_, client)).toSet
    if(tile != null && tile.isInstanceOf[TileMultipart])
    {
      var tileTraits = tileTraitMap(tile.getClass.getName)
      if(partTraits.forall(tileTraits(_)) && partTraits.size == tileTraits.size)//equal contents
        return tile.asInstanceOf[TileMultipart]
      
    }
    return SuperSet(partTraits, client)
  }
  
  /**
   * Check if there are any redundant interfaces on tile, if so, replace tile with new copy
   */
  def partRemoved(tile:TileMultipart, part:TMultiPart):TileMultipart = 
  {
    val client = tile.worldObj.isRemote
    var partTraits = tile.partList.flatMap(traitsForPart(_, client))
    var testSet = partTraits.toSet
    if(!traitsForPart(part, client).forall(testSet(_)))
    {
      val ntile = SuperSet(testSet, client)
      tile.worldObj.setBlockTileEntity(tile.xCoord, tile.yCoord, tile.zCoord, ntile)
      ntile.loadFrom(tile)
      return ntile
    }
    return tile
  }
  
  /**
   * register s_trait to be applied to tiles containing parts implementing s_interface
   */
  def registerTrait(s_interface:String, s_trait:String) { registerTrait(s_interface, s_trait, s_trait) }
  
  /**
   * register traits to be applied to tiles containing parts implementing s_interface
   * s_trait for server worlds (may be null)
   * c_trait for client worlds (may be null)
   */
  def registerTrait(s_interface:String, c_trait:String, s_trait:String)
  {
    if(c_trait != null)
    {
      //TODO some checking
      interfaceTraitMap_c(s_interface) = c_trait
    }
    if(s_trait != null)
    {
      //TODO some checking
      interfaceTraitMap_s(s_interface) = s_trait
    }
  }
}
