package codechicken.multipart

import net.minecraft.block.Block
import net.minecraft.client.multiplayer.NetClientHandler
import net.minecraft.client.particle.EffectRenderer
import net.minecraft.entity.item.EntityItem
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.Entity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{ NBTTagCompound, NBTTagList }
import net.minecraft.network.packet.Packet
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.MovingObjectPosition
import net.minecraft.world.ChunkCoordIntPair
import net.minecraft.world.World
import codechicken.core.data.{ MCDataInputStream, MCDataOutput, MCDataOutputStream }
import codechicken.core.lighting.LazyLightMatrix
import codechicken.core.packet.PacketCustom
import codechicken.core.vec.{ BlockCoord, Cuboid6, Vector3 }
import codechicken.multipart.handler.{ MultipartCPH, MultipartProxy, MultipartSPH }
import codechicken.scala.ScalaBridge._
import java.io.{ ByteArrayOutputStream, DataOutputStream, DataInputStream }
import java.util.{ List => JList, Random }
import collection.JavaConverters._
import collection.mutable.{ ArrayBuffer, HashSet, ListBuffer, Map => MMap, Queue }
import cpw.mods.fml.relauncher.{ SideOnly, Side}
import Side.{ SERVER, CLIENT }

case class TileMultipartTrait(val parent: TileMultipart)

class TileMultipart extends TileEntity
{
    val traitMap = MMap[String, TileMultipartTrait]()
    var partList = ArrayBuffer[TMultiPart]()
    var partMap = Array.fill[TMultiPart](27)(null)
    
    private var doesTick = false
    
    def loadFrom(that:TileMultipart)//Potentially auto gen this
    {
        partList = that.partList
        partMap = that.partMap
        doesTick = that.doesTick
        
        for(p <- partList) p.bind(this)
    }
    
    def jPartList():JList[TMultiPart] = partList.asJava
    
    override def canUpdate() = doesTick//TODO: part merging true
    
    override def updateEntity()
    {
        super.updateEntity()
        
        TileMultipart.startOperation(this)
        for(p <- partList) p.update()
        TileMultipart.finishOperation(this)
    }
    
    override def onChunkUnload() = for(p <- partList) p.onChunkUnload()

    def onChunkLoad() = for(p <- partList) p.onChunkLoad()
    
    override def validate()
    {
        val wasInvalid = isInvalid()
        super.validate()
        if(wasInvalid)
            for(p <- partList) p.onWorldJoin()
    }
    
    override def invalidate()
    {
        super.invalidate()
        if(worldObj != null)
            for(p <- partList) p.onWorldSeparate()
    }
    
    def notifyPartChange()
    {
        TileMultipart.startOperation(this)
        for(p <- partList) p.onPartChanged()
        TileMultipart.finishOperation(this)
        
        worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord, getBlockType().blockID)
        worldObj.updateAllLightTypes(xCoord, yCoord, zCoord)
    }
    
    def onNeighborBlockChange(world:World, x:Int, y:Int, z:Int, id:Int)
    {
        TileMultipart.startOperation(this)
        for(p <- partList) p.onNeighbourChanged()
        TileMultipart.finishOperation(this)
    }
    
    def getLightValue() = partList.foldLeft(0)((l, p) => Math.max(l, p.getLightValue))
    
    def markDirty() = worldObj.updateTileEntityChunkAndDoNothing(xCoord, yCoord, zCoord, this)
    
    def isSolid(side:Int):Boolean = 
    {
        val part = partMap(PartMap.face(side).i)
        if(part != null) part.asInstanceOf[TFacePart].solid(side) else false
    }
    
    private def setTicking(tick:Boolean)
    {
        if(doesTick == tick)
            return
        
        doesTick = tick
        if(worldObj != null)
        {
            if(tick)
                worldObj.addTileEntity(this)
            else
                worldObj.loadedTileEntityList.remove(this)
        }
    }
    
    def canAddPart(part:TMultiPart):Boolean =
    {
        if(partList.contains(part))
            return false
            
        val slotMask = part.getSlotMask
        for(i <- 0 until partMap.length)
            if((slotMask&1<< i) != 0 && partMap(i) != null)
                return false
        
        return occlusionTest(partList, part)
    }
    
    def canReplacePart(opart:TMultiPart, npart:TMultiPart):Boolean = 
    {
        val olist = partList.filterNot(_ == opart)
        if(olist.contains(npart))
            return false
        
        return occlusionTest(olist, npart)
    }
    
    def occlusionTest(parts:Seq[TMultiPart], npart:TMultiPart):Boolean =
    {
        return parts.forall(part => part.occlusionTest(npart) && npart.occlusionTest(part))
    }
    
    def getWriteStream(part:TMultiPart):MCDataOutput = getWriteStream.writeByte(partList.indexOf(part))
    
    private[multipart] def getWriteStream = MultipartSPH.getTileStream(worldObj, new BlockCoord(this))
    
    private[multipart] def addPart(part:TMultiPart)
    {
        addPart_do(part)
        part.onAdded()
        notifyPartChange()
        markDirty()
        markRender()
        
        if(!worldObj.isRemote)
            writeAddPart(part)
    }
    
    private[multipart] def writeAddPart(part:TMultiPart)
    {
        val stream = getWriteStream.writeByte(253)
        MultiPartRegistry.writePartID(stream, part)
        part.writeDesc(stream)
    }
    
    private[multipart] def addPart_do(part:TMultiPart)
    {
        if(partList.size >= 250)
            throw new IllegalArgumentException("Tried to add more than 250 parts to the one tile. You're doing it wrong")
        
        partList+=part
        val mask = part.getSlotMask
        for(i <- 0 until 27)
            if ((mask&1<< i) > 0)
                partMap(i) = part
        
        part.bind(this)
        
        if(!doesTick && part.doesTick)
            setTicking(true)
        
        partAdded(part)
    }
    
    def partAdded(part:TMultiPart){}
    
    def remPart(part:TMultiPart):TileMultipart =
    {
        if(TileMultipart.queueRemoval(this, part))
            return null
        
        val i = remPart_do(part)
        if(!isInvalid())
        {
            notifyPartChange()
            markDirty()
            markRender()
        }
        
        if(!worldObj.isRemote)
            getWriteStream.writeByte(254).writeByte(i)
        
        if(!isInvalid())
            return MultipartGenerator.partRemoved(this, part)
        
        return null
    }
    
    private def remPart_do(part:TMultiPart):Int =
    {
        val r = partList.indexOf(part)
        if(r < 0)
            throw new IllegalArgumentException("Tried to remove a non-existant part")
        
        partList-=part
        for(i <- 0 until 27)
            if(partMap(i) == part)
                partMap(i) = null
        
        partRemoved(part, r)
        part.onRemoved()
        
        if(partList.isEmpty)
        {
            worldObj.setBlockToAir(xCoord, yCoord, zCoord)
        }
        else
        {
            if(part.doesTick && doesTick)
            {
                var ntick = false
                partList.foreach(part => ntick |= part.doesTick)
                if(!ntick)
                    setTicking(false)
            }
        }
        return r
    }
    
    def partRemoved(part:TMultiPart, p:Int){}

    private[multipart] def loadParts(parts:ListBuffer[TMultiPart])
    {
        clearParts()
        for(i <- 0 until partMap.length)
            partMap(i) = null
        parts.foreach(p => addPart_do(p))
        if(worldObj != null)
            notifyPartChange()
    }
    
    def clearParts()
    {
        partList.clear()
    }
    
    def writeDesc(packet:PacketCustom)
    {
        packet.writeByte(partList.size)
        partList.foreach{part =>
            MultiPartRegistry.writePartID(packet, part)
            part.writeDesc(packet)
        }
    }
    
    def harvestPart(index:Int, drop:Boolean):Boolean = 
    {
        val part = partList(index)
        if(part == null)
            return false
        if(drop)
            dropItems(part.getDrops)
        remPart(part)
        return true
    }
    
    def dropItems(items:Seq[ItemStack])
    {
        val pos = Vector3.fromTileEntityCenter(this)
        items.foreach(item => TileMultipart.dropItem(item, worldObj, pos))
    }
    
    def markRender()
    {
        worldObj.markBlockForRenderUpdate(xCoord, yCoord, zCoord)
    }
    
    override def writeToNBT(tag:NBTTagCompound)
    {
        super.writeToNBT(tag)
        val taglist = new NBTTagList
        partList.foreach{part => 
            val parttag = new NBTTagCompound
            parttag.setString("id", part.getType)
            part.save(parttag)
            taglist.appendTag(parttag)
    }
        tag.setTag("parts", taglist)
    }
    
    def onEntityCollision(entity:Entity)
    {
        TileMultipart.startOperation(this)
        for(p <- partList) p.onEntityCollision(entity)
        TileMultipart.finishOperation(this)
    }
    
    def strongPowerLevel(side:Int) = 0
    
    def weakPowerLevel(side:Int) = 0
    
    def canConnectRedstone(side:Int) = false
    
    def notifyNeighborChange(side:Int)
    {
        val pos = new BlockCoord(this).offset(side)
        worldObj.notifyBlocksOfNeighborChange(pos.x, pos.y, pos.z, getBlockType().blockID)
    }
}

class TileMultipartClient extends TileMultipart
{
    def renderStatic(pos:Vector3, olm:LazyLightMatrix, pass:Int)
    {
        for(p <- partList) p.renderStatic(pos, olm, pass)
    }
    
    def renderDynamic(pos:Vector3, frame:Float)
    {
        for(p <- partList) p.renderDynamic(pos, frame)
    }
    
    def randomDisplayTick(random:Random){}
}

object TileMultipart
{
    var renderID:Int = -1
    
    class TileOperationSet
    {
        var tile:TileMultipart = _
        var depth = 0
        private val removalQueue = Queue[TMultiPart]()
        private val additionQueue = Queue[TMultiPart]()
        
        def start(t:TileMultipart)
        {
            tile = t
            depth = 1
        }
        
        def queueAddition(part:TMultiPart)
        {
            if(!additionQueue.contains(part))
                additionQueue+=part
        }
        
        def queueRemoval(part:TMultiPart)
        {
            if(!removalQueue.contains(part))
                removalQueue+=part
        }
        
        def finish()
        {
            if(removalQueue.isEmpty && additionQueue.isEmpty)
                return
            
            var otile = tile
            val world = tile.worldObj
            val pos = new BlockCoord(tile)
            while(!removalQueue.isEmpty)
                otile = otile.remPart(removalQueue.dequeue)
            
            while(!additionQueue.isEmpty)
                MultipartGenerator.addPart(world, pos, additionQueue.dequeue)
        }
    }
    
    class OperationSynchroniser
    {
        private val operations = ArrayBuffer[TileOperationSet]()
        private var depth = 0
        
        def startOperation(tile:TileMultipart)
        {
            var i = 0
            while(i < depth)
            {
                val op = operations(i)
                if(op.tile == tile)
                {
                    op.depth+=1
                    return
                }
                i+=1
            }
            if(depth == operations.length)
                operations+=new TileOperationSet
            operations(depth).start(tile)
            depth+=1
        }
        
        def finishOperation(tile:TileMultipart)
        {
            var i = depth-1
            while(i >= 0)
            {
                val op = operations(i)
                if(op.tile == tile)
                {
                    op.depth-=1
                    if(op.depth == 0)
                    {
                        if(i != depth-1)
                            throw new IllegalStateException("Tried to finish an operation that was not on top")
                        depth-=1
                        op.finish()
                    }
                    return
                }
                i-=1
            }
            throw new IllegalStateException("Inconsistant Operation stack")
        }
        
        def queueRemoval(tile:TileMultipart, part:TMultiPart):Boolean =
        {
            var i = depth-1
            while(i >= 0)
            {
                val op = operations(i)
                if(op.tile == tile)
                {
                    op.queueRemoval(part)
                    return true
                }
                i-=1
            }
            return false
        }
        
        def queueAddition(world:World, pos:BlockCoord, part:TMultiPart):Boolean = 
        {
            var i = depth-1
            while(i >= 0)
            {
                val op = operations(i)
                val opt = op.tile
                if(opt.worldObj == world && opt.xCoord == pos.x && opt.yCoord == pos.y && opt.zCoord == pos.z)
                {
                    op.queueRemoval(part)
                    return true
                }
                i-=1
            }
            return false
        }
    }
    
    private var operationSync_ = new ThreadLocal[OperationSynchroniser]
    private def operationSync():OperationSynchroniser = 
    {
        var r = operationSync_.get
        if(r == null)
        {
            r = new OperationSynchroniser
            operationSync_.set(r)
        }
        return r
    }
    
    def startOperation(tile:TileMultipart) = operationSync.startOperation(tile)
    
    def finishOperation(tile:TileMultipart) = operationSync.finishOperation(tile)
        
    def queueRemoval(tile:TileMultipart, part:TMultiPart):Boolean = operationSync.queueRemoval(tile, part)
    
    def queueAddition(world:World, pos:BlockCoord, part:TMultiPart):Boolean = operationSync.queueAddition(world, pos, part)
    
    def getOrConvertTile(world:World, pos:BlockCoord) = getOrConvertTile2(world, pos)._1
    
    def getOrConvertTile2(world:World, pos:BlockCoord):(TileMultipart, Boolean) =
    {
        val side = if(world.isRemote) CLIENT else SERVER
        val t = world.getBlockTileEntity(pos.x, pos.y, pos.z)
        if(t.isInstanceOf[TileMultipart])
            return (t.asInstanceOf[TileMultipart], false)
        
        val id = world.getBlockId(pos.x, pos.y, pos.z)
        val p = MultiPartRegistry.convertBlock(world, pos, id)
        if(p != null)
        {
            val t = MultipartGenerator.generateCompositeTile(null, Seq(p), side)
            t.xCoord = pos.x
            t.yCoord = pos.y
            t.zCoord = pos.z
            t.setWorldObj(world)
            t.addPart_do(p)
            return (t, true)
        }
        return (null, false)
    }
    
    def getTile(world:World, pos:BlockCoord):TileMultipart =
    {
        val t = world.getBlockTileEntity(pos.x, pos.y, pos.z)
        if(t.isInstanceOf[TileMultipart])
            return t.asInstanceOf[TileMultipart]
        return null
    }
    
    def canAddPart(world:World, pos:BlockCoord, part:TMultiPart):Boolean =
    {
        part.getCollisionBoxes.foreach{b => 
            if(!world.checkNoEntityCollision(b.toAABB().offset(pos.x, pos.y, pos.z)))
                return false
        }
        
        val t = getOrConvertTile(world, pos)
        if(t != null)
            return t.canAddPart(part)
        
        if(!replaceable(world, pos))
            return false
        
        return true
    }
    
    def replaceable(world:World, pos:BlockCoord):Boolean = 
    {
        val block = Block.blocksList(world.getBlockId(pos.x, pos.y, pos.z))
        return block == null || block.isAirBlock(world, pos.x, pos.y, pos.z) || block.isBlockReplaceable(world, pos.x, pos.y, pos.z)
    }
    
    def addPart(world:World, pos:BlockCoord, part:TMultiPart):TileMultipart =
    {
        if(queueAddition(world, pos, part))
            return null
        
        return MultipartGenerator.addPart(world, pos, part)
    }
    
    def handleDescPacket(world:World, pos:BlockCoord, packet:PacketCustom)
    {
        val nparts = packet.readUnsignedByte
        val parts = new ListBuffer[TMultiPart]()
        for(i <- 0 until nparts)
        {
            val part:TMultiPart = MultiPartRegistry.readPart(packet)
            part.readDesc(packet)
            parts+=part
        }
        
        if(parts.size == 0)
            return
        
        val t = world.getBlockTileEntity(pos.x, pos.y, pos.z)
        val tilemp = MultipartGenerator.generateCompositeTile(t, parts, CLIENT)
        if(tilemp != t)
            world.setBlockTileEntity(pos.x, pos.y, pos.z, tilemp)
        
        tilemp.loadParts(parts)
        tilemp.markRender()
    }
    
    def handlePacket(pos:BlockCoord, world:World, i:Int, packet:PacketCustom)
    {
        var tilemp = BlockMultipart.getTile(world, pos.x, pos.y, pos.z)
        
        i match
        {
            case 253 => {
                val part = MultiPartRegistry.readPart(packet)
                part.readDesc(packet)
                addPart(world, pos, part)
            }
            case 254 => if(tilemp != null) {
                tilemp.remPart(tilemp.partList(packet.readUnsignedByte()))
            }
            case _ => if(tilemp != null) {
                tilemp.partList(i).read(packet)
            }
        }
    }
    
    def createFromNBT(tag:NBTTagCompound):TileMultipart =
    {
        val superID = tag.getString("superID")
        //val superClass = TileEntity.nameToClassMap.get(superID)
        
        val partList = tag.getTagList("parts")
        val parts = ListBuffer[TMultiPart]()
        
        for(i <- 0 until partList.tagCount)
        {
            val partTag = partList.tagAt(i).asInstanceOf[NBTTagCompound]
            val partID = partTag.getString("id")
            val part = MultiPartRegistry.createPart(partID, false)
            if(part != null)
            {
                part.load(partTag)
                parts+=part
            }
        }
        
        if(parts.size == 0)
            return null
        
        val tmb = MultipartGenerator.generateCompositeTile(null, parts, SERVER)
        tmb.readFromNBT(tag)
        tmb.loadParts(parts)
        return tmb
    }
    
    def dropItem(stack:ItemStack, world:World, pos:Vector3)
    {
        val item = new EntityItem(world, pos.x, pos.y, pos.z, stack);
        item.motionX = world.rand.nextGaussian() * 0.05;
        item.motionY = world.rand.nextGaussian() * 0.05 + 0.2F;
        item.motionZ = world.rand.nextGaussian() * 0.05;
        world.spawnEntityInWorld(item);
    }
}
