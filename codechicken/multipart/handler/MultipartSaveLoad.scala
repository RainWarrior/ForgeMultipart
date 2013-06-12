package codechicken.multipart.handler

import net.minecraft.tileentity.TileEntity
import net.minecraft.nbt.NBTTagCompound
import java.util.Map
import net.minecraft.world.chunk.Chunk
import net.minecraft.world.ChunkPosition
import codechicken.multipart.TileMultipart


/**
 * Hack due to lack of TileEntityLoadEvent in forge
 */
object MultipartSaveLoad
{
    class TileNBTContainer extends TileEntity
    {
        var tag:NBTTagCompound = _
        
        override def readFromNBT(t:NBTTagCompound)
        {
            super.readFromNBT(t)
            tag = t
        }
    }
    
    hookLoader()
    def hookLoader()
    {
        val field = classOf[TileEntity].getDeclaredFields()(0)
        field.setAccessible(true)
        val map = field.get(null).asInstanceOf[Map[String, Class[_ <: TileEntity]]]
        map.put("savedMultipart", classOf[TileNBTContainer])
    }
    
    private val classToNameMap = getClassToNameMap
    def registerTileClass(t:Class[_ <: TileEntity])
    {
        classToNameMap.put(t, "savedMultipart")
    }
    
    def getClassToNameMap() =
    {
        val field = classOf[TileEntity].getDeclaredFields()(1)
        field.setAccessible(true)
        field.get(null).asInstanceOf[Map[Class[_ <: TileEntity], String]]
    }
    
    def loadTiles(chunk:Chunk)
    {
        val iterator = chunk.chunkTileEntityMap.asInstanceOf[Map[ChunkPosition, TileEntity]].entrySet.iterator
        while(iterator.hasNext)
        {
            val e = iterator.next
            if(e.getValue.isInstanceOf[TileNBTContainer])
            {
                val t = TileMultipart.createFromNBT(e.getValue.asInstanceOf[TileNBTContainer].tag)
                if(t != null)
                {
                    t.setWorldObj(e.getValue.worldObj)
                    t.validate
                    e.setValue(t)
                }
                else
                    iterator.remove
            }
        }
    }
}
