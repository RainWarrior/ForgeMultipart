package codechicken.multipart

import scala.collection.mutable.ArrayBuffer
import java.util.Random

trait IRandomDisplayTick
{
    def randomDisplayTick(random:Random)
}

/**
 * Saves processor time looping on tiles that don't need it
 */
class TRandomDisplayTickTile(parent: TileMultipart) extends TileMultipartTrait(parent) with IRandomDisplayTick
{
    override def randomDisplayTick(random:Random)
    {
        parent.partList.filter(_.isInstanceOf[IRandomDisplayTick]).map(
            _.asInstanceOf[IRandomDisplayTick].randomDisplayTick(random))
    }
}
