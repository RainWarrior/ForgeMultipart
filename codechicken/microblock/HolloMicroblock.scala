package codechicken.microblock

import net.minecraft.world.World
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.MovingObjectPosition
import codechicken.multipart.TFacePart
import codechicken.core.vec.BlockCoord
import codechicken.core.vec.Cuboid6
import codechicken.core.raytracer.SelectionBox
import codechicken.core.vec.Rotation
import codechicken.core.vec.Vector3
import codechicken.multipart.TNormalOcclusion
import codechicken.core.lighting.LazyLightMatrix
import codechicken.microblock.MicroMaterialRegistry.IMicroMaterial
import net.minecraft.client.renderer.RenderBlocks
import codechicken.core.render.IconTransformation
import codechicken.core.render.RenderUtils
import codechicken.core.render.CCRenderState
import codechicken.scala.ScalaBridge._
import codechicken.scala.JSeq

object HollowPlacement extends PlacementProperties
{
    def microClass = HollowMicroClass
    
    def placementGrid = FacePlacementGrid
    
    def opposite(slot:Int, side:Int) = slot^1
    
    override def expand(slot:Int, side:Int) = sneakOpposite(slot, side)
    
    override def sneakOpposite(slot:Int, side:Int) = slot == (side^1)
}

object HollowMicroClass extends MicroblockClass
{
    var pBoxes:Array[Seq[Cuboid6]] = new Array(256)
    var occBounds:Array[Cuboid6] = new Array(256)
    for(s <- 0 until 6)
    {
        val transform = Rotation.sideRotations(s).at(Vector3.center)
        for(t <- 1 until 8)
        {
            val d = t/8D
            val w1 = 1/8D
            val w2 = 3/16D
            pBoxes(t<<4|s) = Seq(
                new SelectionBox(new Cuboid6(0, 0, 0, w1, d, 1)),
                new SelectionBox(new Cuboid6(1-w1, 0, 0, 1, d, 1)),
                new SelectionBox(new Cuboid6(w1, 0, 0, 1-w1, d, w1)),
                new SelectionBox(new Cuboid6(w1, 0, 1-w1, 1-w1, d, 1)))
                .map(_.transform(transform).bound)
            occBounds(t<<4|s) = new SelectionBox(new Cuboid6(1/8D, 0, 1/8D, 7/8D, d, 7/8D))
                .transform(transform).bound
        }
    }
    
    def getName = "mcr_hllw"
    
    def create(client:Boolean) = 
        if(client)
            new HollowMicroblockClient
        else
            new HollowMicroblock
    
    def create(size:Int, slot:Int, material:Int, client:Boolean) = 
        if(client)
            new HollowMicroblockClient(size, slot, material)
        else
            new HollowMicroblock(size, slot, material)
    
    def placementProperties = HollowPlacement
    
    def getDisplayName(size:Int) = "Hollow "+FaceMicroClass.getDisplayName(size)
}

class HollowMicroblockClient(shape$:Byte = 0, material$:Int = 0) extends HollowMicroblock(shape$, material$) with CommonMicroblockClient
{
    renderMask |= 8<<8
    
    def this(size:Int, shape:Int, material:Int) = this((size<<4|shape).toByte, material)
    
    override def onPartChanged()
    {
        super.onPartChanged()
        renderMask = renderMask&0xFF | getHollowSize<<8
    }
    
    override def drawBreaking(renderBlocks:RenderBlocks)
    {
        CCRenderState.reset()
        val icont = new IconTransformation(renderBlocks.overrideBlockTexture)
        renderHollow(Vector3.fromTileEntity(tile), null, null, getBounds, 0, false, 
            (pos:Vector3, olm:LazyLightMatrix, mat:IMicroMaterial, c:Cuboid6, sideMask:Int)=>
                RenderUtils.renderBlock(c, sideMask, pos, null, -1, icont))
    }
    
    override def render(pos:Vector3, olm:LazyLightMatrix, mat:IMicroMaterial, c:Cuboid6, sideMask:Int)
    {
        if(isTransparent)
            renderHollow(pos, olm, mat, c, sideMask, false, renderCuboid)
        else
        {
            renderHollow(pos, olm, mat, c, sideMask|1<<getSlot, false, renderCuboid)
            renderHollow(pos, olm, mat, Cuboid6.full, sideMask, true, renderCuboid)
        }
    }
    
    def renderHollow(pos:Vector3, olm:LazyLightMatrix, mat:IMicroMaterial, c:Cuboid6, sideMask:Int, face:Boolean, 
            f:(Vector3, LazyLightMatrix, IMicroMaterial, Cuboid6, Int)=>Unit)
    {
        val size = renderMask >> 8
        val d1 = 0.5-size/32D
        val d2 = 0.5+size/32D
        val x1 = c.min.x
        val x2 = c.max.x
        val y1 = c.min.y
        val y2 = c.max.y
        val z1 = c.min.z
        val z2 = c.max.z
        
        var faceMask = sideMask
        var iMask = 0
        if(face)
            faceMask = ~(1<<getSlot)
        
        getSlot match 
        {
            case 0 | 1 => 
            {
                if(face)
                    iMask = 0x3C
                f(pos, olm, mat, new Cuboid6(d1, y1, d2, d2, y2, z2), 0x3B|iMask)//-z internal
                f(pos, olm, mat, new Cuboid6(d1, y1, z1, d2, y2, d1), 0x37|iMask)//+z internal
                
                f(pos, olm, mat, new Cuboid6(d2, y1, d1, x2, y2, d2), faceMask&0x23|0xC|iMask)//-x internal -y+y+x external
                f(pos, olm, mat, new Cuboid6(x1, y1, d1, d1, y2, d2), faceMask&0x13|0xC|iMask)//+x internal -y+y-x external
                
                f(pos, olm, mat, new Cuboid6(x1, y1, d2, x2, y2, z2), faceMask&0x3B|4|iMask)//-y+y+z-x+x external
                f(pos, olm, mat, new Cuboid6(x1, y1, z1, x2, y2, d1), faceMask&0x37|8|iMask)//-y+y-z-x+x external
            }
            case 2 | 3 =>
            {
                if(face)
                    iMask = 0x33
                f(pos, olm, mat, new Cuboid6(d2, d1, z1, x2, d2, z2), 0x2F|iMask)//-x internal
                f(pos, olm, mat, new Cuboid6(x1, d1, z1, d1, d2, z2), 0x1F|iMask)//+x internal
                
                f(pos, olm, mat, new Cuboid6(d1, d2, z1, d2, y2, z2), faceMask&0xE|0x30|iMask)//-y internal -z+z+y external
                f(pos, olm, mat, new Cuboid6(d1, y1, z1, d2, d1, z2), faceMask&0xD|0x30|iMask)//+y internal -z+z-y external
                
                f(pos, olm, mat, new Cuboid6(d2, y1, z1, x2, y2, z2), faceMask&0x2F|0x10|iMask)//-z+z+x-y+y external
                f(pos, olm, mat, new Cuboid6(x1, y1, z1, d1, y2, z2), faceMask&0x1F|0x20|iMask)//-z+z-x-y+y external
            }
            case 4 | 5 => 
            {
                if(face)
                    iMask = 0xF
                f(pos, olm, mat, new Cuboid6(x1, d2, d1, x2, y2, d2), 0x3E|iMask)//-y internal
                f(pos, olm, mat, new Cuboid6(x1, y1, d1, x2, d1, d2), 0x3D|iMask)//+y internal
                
                f(pos, olm, mat, new Cuboid6(x1, d1, d2, x2, d2, z2), faceMask&0x38|3|iMask)//-z internal -x+x+z external
                f(pos, olm, mat, new Cuboid6(x1, d1, z1, x2, d2, d1), faceMask&0x34|3|iMask)//+z internal -x+x-z external
                
                f(pos, olm, mat, new Cuboid6(x1, d2, z1, x2, y2, z2), faceMask&0x3E|1|iMask)//-x+x+y-z+z external
                f(pos, olm, mat, new Cuboid6(x1, y1, z1, x2, d1, z2), faceMask&0x3D|2|iMask)//-x+x-y-z+z external
            }
        }
    }
}

class HollowMicroblock(shape$:Byte = 0, material$:Int = 0) extends CommonMicroblock(shape$, material$) with TFacePart with TNormalOcclusion
{
    def this(size:Int, shape:Int, material:Int) = this((size<<4|shape).toByte, material)

    def microClass = HollowMicroClass
    
    def getBounds:Cuboid6 = FaceMicroClass.aBounds(shape)
    
    override def getPartialOcclusionBoxes = HollowMicroClass.pBoxes(shape)
    
    def getHollowSize = 
    {
        val part = tile.partMap(6)
        if(part.isInstanceOf[IHollowConnect])
            part.asInstanceOf[IHollowConnect].getSize
        else
            8
    }
    
    def getOcclusionBoxes:JSeq[Cuboid6] = 
    {
        val size = getSize
        val c = HollowMicroClass.occBounds(shape)
        val d1 = 0.5-size/32D
        val d2 = 0.5+size/32D
        val x1 = c.min.x
        val x2 = c.max.x
        val y1 = c.min.y
        val y2 = c.max.y
        val z1 = c.min.z
        val z2 = c.max.z
        
        return getSlot match 
        {
            case 0 | 1 => 
            {
                Seq(new Cuboid6(d2, y1, d1, x2, y2, d2),
                new Cuboid6(x1, y1, d1, d1, y2, d2),
                new Cuboid6(x1, y1, d2, x2, y2, z2),
                new Cuboid6(x1, y1, z1, x2, y2, d1))
            }
            case 2 | 3 =>
            {
                Seq(new Cuboid6(d1, d2, z1, d2, y2, z2),
                new Cuboid6(d1, y1, z1, d2, d1, z2),
                new Cuboid6(d2, y1, z1, x2, y2, z2),
                new Cuboid6(x1, y1, z1, d1, y2, z2))
            }
            case 4 | 5 => 
            {
                Seq(new Cuboid6(x1, d1, d2, x2, d2, z2),
                new Cuboid6(x1, d1, z1, x2, d2, d1),
                new Cuboid6(x1, d2, z1, x2, y2, z2),
                new Cuboid6(x1, y1, z1, x2, d1, z2))
            }
        }
    }
    
    override def allowCompleteOcclusion = true
    
    override def solid(side:Int) = false
}