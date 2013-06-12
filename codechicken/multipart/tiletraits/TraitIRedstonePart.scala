package codechicken.multipart

import net.minecraft.world.World
import net.minecraft.block.Block
import net.minecraft.block.BlockRedstoneWire
import net.minecraft.world.IBlockAccess
import net.minecraft.util.Direction
import PartMap._
import codechicken.core.vec.Rotation._

trait IRedstonePart
{
    def strongPowerLevel(side:Int):Int
    def weakPowerLevel(side:Int):Int
    def canConnectRedstone(side:Int):Boolean
}

/**
 * For parts like wires that adhere to a specific face, reduces redstone connections to the specific edge between two faces
 */
trait IFaceRedstonePart extends IRedstonePart
{
    def getFace():Int
}

trait IRedstoneTile extends IRedstoneConnector {
  def openConnections(side: Int): Int
}

class TRedstoneTile(parent: TileMultipart) extends TileMultipartTrait(parent) with IRedstoneTile
{
    import RedstoneInteractions._
    import parent._
    
    def strongPowerLevel(side:Int):Int =
    {
        var max = 0
        partList.foreach(p => 
            if(p.isInstanceOf[IRedstonePart])
            {
                val l = p.asInstanceOf[IRedstonePart].strongPowerLevel(side)
                if(l > max) max = l
            }
        )
        return max
    }
    
    override def openConnections(side:Int):Int =
    {
        if(blocksRedstone(side))
            return 0
        
        var m = 0x10
        var i = 0
        while(i < 4)
        {
            if(!blocksRedstone(edgeBetween(side, rotateSide(side&6, i))))
                m |= 1 << i
            i+=1
        }
        return m
    }
    
    def blocksRedstone(i:Int) = partMap(i) != null && partMap(i).blocksRedstone
    
    def weakPowerLevel(side:Int):Int = 
        weakPowerLevel(side, otherConnectionMask(worldObj, xCoord, yCoord, zCoord, side, true))
    
    def canConnectRedstone(side:Int):Boolean =
    {
        val vside = vanillaToSide(side)
        return (getConnectionMask(vside) & otherConnectionMask(worldObj, xCoord, yCoord, zCoord, vside, false)) > 0
    }
    
    def getConnectionMask(side:Int):Int = 
    {
        val mask = openConnections(side)
        var res = 0
        partList.foreach(p => 
            res|=connectionMask(p, side)&mask)
        return res
    }
    
    def weakPowerLevel(side:Int, mask:Int):Int = 
    {
        val tmask = openConnections(side)&mask
        var max = 0
        partList.foreach(p => 
            if((connectionMask(p, side)&tmask) > 0)
            {
                val l = p.asInstanceOf[IRedstonePart].weakPowerLevel(side)
                if(l > max) max = l
            })
        return max
    }
}

/**
 * All connection masks are a 5 bit map. 
 * The lowest 4 bits correspond to the connection toward the face specfied rotateSide(side&6, b) where b is the bit index from lowest to highest.
 * Bit 5 corresponds to a connection opposite side.
 */
trait IRedstoneConnector
{
    def getConnectionMask(side:Int):Int
    def weakPowerLevel(side:Int, mask:Int):Int
}

trait IRedstoneConnectorBlock
{
    def getConnectionMask(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int):Int
    def weakPowerLevel(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int, mask:Int):Int
}

object RedstoneInteractions
{
    import net.minecraft.util.Facing._
    
    val vanillaSideMap = Array(-2, -1, 0, 2, 3, 1)
    val sideVanillaMap = Array(1, 2, 5, 3, 4)
    
    def getWeakPowerTo(p:TMultiPart, side:Int):Int =
    {
        val tile = p.tile
        return getWeakPowerTo(tile.worldObj, tile.xCoord, tile.yCoord, tile.zCoord, side, 
                tile.asInstanceOf[IRedstoneTile].openConnections(side)&connectionMask(p, side))
    }
    
    def getWeakPowerTo(world:World, x:Int, y:Int, z:Int, side:Int, mask:Int):Int =
        getWeakPower(world, x+offsetsXForSide(side), y+offsetsYForSide(side), z+offsetsZForSide(side), side^1, mask)
        
    def getWeakPower(world:World, x:Int, y:Int, z:Int, side:Int, mask:Int):Int =
    {
        val tile = world.getBlockTileEntity(x, y, z)
        if(tile.isInstanceOf[IRedstoneConnector])
            return tile.asInstanceOf[IRedstoneConnector].weakPowerLevel(side, mask)
        
        val block = Block.blocksList(world.getBlockId(x, y, z))
        if(block == null)
            return 0
        
        if(block.isInstanceOf[IRedstoneConnectorBlock])
            return block.asInstanceOf[IRedstoneConnectorBlock].weakPowerLevel(world, x, y, z, side, mask)
        
        val vmask = vanillaConnectionMask(block, world, x, y, z, side, true)
        if((vmask&mask) > 0)
            return world.getIndirectPowerLevelTo(x, y, z, side^1)
        return 0
    }
    
    def vanillaToSide(vside:Int) = sideVanillaMap(vside+1)
    
    def otherConnectionMask(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int, power:Boolean):Int =
        getConnectionMask(world, x+offsetsXForSide(side), y+offsetsYForSide(side), z+offsetsZForSide(side), side^1, power)
        
    def connectionMask(p:TMultiPart, side:Int):Int =
    {
        if(p.isInstanceOf[IRedstonePart] && p.asInstanceOf[IRedstonePart].canConnectRedstone(side))
        {
            if(p.isInstanceOf[IFaceRedstonePart])
                return rotationTo(side, p.asInstanceOf[IFaceRedstonePart].getFace)
            return 0x1F
        }
        return 0
    }
    
    /**
     * @param power If true, don't test canConnectRedstone on blocks, just get a power transmission mask rather than a visual connection
     */
    def getConnectionMask(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int, power:Boolean):Int =
    {
        val tile = world.getBlockTileEntity(x, y, z)
        if(tile.isInstanceOf[IRedstoneConnector])
            return tile.asInstanceOf[IRedstoneConnector].getConnectionMask(side)
        
        val block = Block.blocksList(world.getBlockId(x, y, z))
        if(block == null)
            return 0
        
        if(block.isInstanceOf[IRedstoneConnectorBlock])
            return block.asInstanceOf[IRedstoneConnectorBlock].getConnectionMask(world, x, y, z, side)
        
        return vanillaConnectionMask(block, world, x, y, z, side, power)
    }
    
    def vanillaConnectionMask(block:Block, world:IBlockAccess, x:Int, y:Int, z:Int, side:Int, power:Boolean):Int =
    {
        if(side == 0)//vanilla doesn't handle side 0
        {
            if(power)
                return 0x1F
            return 0
        }
        
        if(block == Block.redstoneWire)
        {
            if(side != 1)
                return 4
            return 0x1F
        }
        
        if(block == Block.redstoneComparatorActive || block == Block.redstoneComparatorIdle)
        {
            if(side != 1)
                return 4
            return 0
        }
        
        val vside = vanillaSideMap(side)
        if(block == Block.redstoneRepeaterActive || block == Block.redstoneRepeaterIdle)//stupid minecraft hardcodes
        {
             val meta = world.getBlockMetadata(x, y, z);
             if(vside == (meta & 3) || vside == Direction.rotateOpposite(meta & 3))
                 return 4
             return 0
        }
        
        if(power || block.canConnectRedstone(world, x, y, z, vside))//some blocks accept power without visualising connections
            return 0x1F
        
        return 0
    }
}
