package codechicken.microblock

import net.minecraft.nbt.NBTTagCompound
import codechicken.core.lighting.LightMatrix
import codechicken.core.packet.PacketCustom
import codechicken.microblock.MicroMaterialRegistry._
import codechicken.core.vec.BlockCoord
import org.lwjgl.opengl.GL11
import codechicken.core.render.CCRenderState
import net.minecraft.world.World
import codechicken.core.vec.Cuboid6
import codechicken.core.vec.Vector3
import codechicken.core.lighting.LazyLightMatrix
import codechicken.multipart.TIconHitEffects
import net.minecraft.util.Icon
import net.minecraft.block.Block
import codechicken.multipart.TCuboidPart
import net.minecraft.util.MovingObjectPosition
import net.minecraft.entity.player.EntityPlayer
import codechicken.multipart.IPartialOcclusion
import codechicken.core.vec.Rotation
import codechicken.multipart.TileMultipart
import codechicken.core.render.Vertex5
import net.minecraft.item.ItemStack
import scala.collection.mutable.ListBuffer
import codechicken.multipart.TMultiPart
import codechicken.scala.JSeq
import codechicken.scala.ScalaBridge._
import codechicken.core.data.MCDataOutput
import codechicken.core.data.MCDataInput
import codechicken.core.render.RenderUtils
import codechicken.core.render.IFaceRenderer

object CommonMicroblock
{
    val face = Array[Vertex5](new Vertex5, new Vertex5, new Vertex5, new Vertex5)
    
    def renderHighlight(world:World, pos:BlockCoord, part:MicroblockClient)
    {
        GL11.glPushMatrix()
        GL11.glTranslated(pos.x+0.5, pos.y+0.5, pos.z+0.5)
        GL11.glScaled(1.002, 1.002, 1.002)
        GL11.glTranslated(-0.5, -0.5, -0.5)
        
        GL11.glEnable(GL11.GL_BLEND)
        GL11.glDepthMask(false)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
        
        CCRenderState.reset()
        CCRenderState.changeTexture("/terrain.png")
        CCRenderState.useNormals(true)
        CCRenderState.setBrightness(world, pos.x, pos.y, pos.z)
        CCRenderState.setAlpha(80)
        CCRenderState.useModelColours(true)
        CCRenderState.startDrawing(7)
        part.render(new Vector3(), null, MicroMaterialRegistry.getMaterial(part.material), part.getBounds, 0)
        CCRenderState.draw()
        
        GL11.glDisable(GL11.GL_BLEND)
        GL11.glDepthMask(true)
        GL11.glPopMatrix()
    }
}

trait MicroblockClient extends Microblock with TIconHitEffects
{
import CommonMicroblock._
    
    def getBrokenIcon(side:Int):Icon = 
    {
        val mat = MicroMaterialRegistry.getMaterial(material)
        if(mat != null)
            return mat.getBreakingIcon(side)
        return Block.stone.getIcon(0, 0)
    }
    
    def render(pos:Vector3, olm:LazyLightMatrix, mat:IMicroMaterial, c:Cuboid6, sideMask:Int)
    {
        renderCuboid(pos, olm, mat, c, sideMask)
    }
    
    def renderCuboid(pos:Vector3, olm:LazyLightMatrix, mat:IMicroMaterial, c:Cuboid6, sideMask:Int)
    {
        var lightMatrix:LightMatrix = null
        if(olm != null)
            lightMatrix = olm.lightMatrix
            
        RenderUtils.renderBlock(c, sideMask, pos, lightMatrix, 
            new IFaceRenderer()
            {
                def renderFace(face:Array[Vertex5], side:Int, pos:Vector3, lightMatrix:LightMatrix) = 
                    mat.renderMicroFace(face, side, pos, lightMatrix, MicroblockClient.this)
            })
    }
}

abstract class Microblock(var shape:Byte = 0, var material:Int = 0) extends TCuboidPart
{    
    def this(size:Int, shape:Int, material:Int) = this((size<<4|shape).toByte, material)
    
    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer):Float = 
    {
        val mat = MicroMaterialRegistry.getMaterial(material)
        if(mat != null)
            return mat.getStrength(player)
        return super.getStrength(hit, player)
    }
    
    override def doesTick = false
    
    def getSize = shape>>4
    
    def getShape = shape&0xF
    
    def getMaterial = material
    
    def itemSizes():Seq[Int] = Seq()
    
    def itemClassID:Int = -1
    
    def sizeToVolume(size:Int):Int = size
    
    override def getDrops():JSeq[ItemStack] =
    {
        var size = getSize
        val items = ListBuffer[ItemStack]()
        for(s <- itemSizes.reverseIterator)
        {
            var m = size/sizeToVolume(s)
            size-=m*sizeToVolume(s)
            while(m > 0)
            {
                val k = Math.min(m, 64)
                m-=k
                items+=ItemMicroPart.create(k, s|itemClassID<<8, MicroMaterialRegistry.materialName(material))
            }
        }
        return items
    }
    
    override def pickItem(hit:MovingObjectPosition):ItemStack = 
    {
        val size = getSize
        for(s <- itemSizes.reverseIterator)
            if(size%s == 0 && size/s >= 1)
                return ItemMicroPart.create(s|itemClassID<<8, MicroMaterialRegistry.materialName(material))
        return null//unreachable
    }
    
    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(shape)
        writePartID(packet, material)
    }
    
    override def readDesc(packet:MCDataInput)
    {
        shape = packet.readByte
        material = readPartID(packet)
    }
    
    override def read(packet:MCDataInput)
    {
        readDesc(packet)
        tile.notifyPartChange()
        tile.markRender()
    }
    
    override def save(tag:NBTTagCompound)
    {
        tag.setByte("shape", shape)
        tag.setString("material", materialName(material))
    }
    
    override def load(tag:NBTTagCompound)
    {
        shape = tag.getByte("shape")
        material = materialID(tag.getString("material"))
    }
    
    def isTransparent = MicroMaterialRegistry.getMaterial(material).isTransparent
    
    override def blocksRedstone = true
}

trait CommonMicroblockClient extends CommonMicroblock with MicroblockClient with TMicroOcclusionClient
{
    override def renderStatic(pos:Vector3, olm:LazyLightMatrix, pass:Int)
    {
        val mat = MicroMaterialRegistry.getMaterial(material)
        if(mat != null && pass == mat.getRenderPass)
            render(pos, olm, mat, renderBounds, renderMask)
    }
}

abstract class CommonMicroblock(shape$:Byte = 0, material$:Int = 0) extends Microblock(shape$, material$) with IPartialOcclusion with TMicroOcclusion
{
    def microClass():MicroblockClass
    
    def getType = microClass.getName
    
    def getSlot = getShape
    
    override def getSlotMask = 1<<getSlot
    
    def getPartialOcclusionBoxes = Seq(getBounds)
    
    override def itemClassID = microClass.classID
    
    override def itemSizes = microClass.itemSizes
    
    override def sizeToVolume(size:Int) = microClass.sizeToVolume(size)
}
