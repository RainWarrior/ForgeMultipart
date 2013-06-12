package codechicken.multipart

trait IPartHandler {
  def partAdded(part:TMultiPart)
  def partRemoved(part:TMultiPart, p:Int)
  def loadFrom(that:TileMultipart)
  def clearParts()
}

class PartHandler(parent: TileMultipart) extends TileMultipartTrait(parent) with IPartHandler {
    override def partAdded(part:TMultiPart)
    {
        parent.partAdded(part)
        parent.traitMap.values.filter(_.isInstanceOf[IPartHandler]).map(_.asInstanceOf[IPartHandler].partAdded(part))
    }
    
    override def partRemoved(part:TMultiPart, p:Int)
    {
        parent.partRemoved(part, p)
        parent.traitMap.values.filter(_.isInstanceOf[IPartHandler]).map(_.asInstanceOf[IPartHandler].partRemoved(part, p))
    }
    
    override def loadFrom(that:TileMultipart)
    {
        parent.loadFrom(that)
        parent.traitMap.values.filter(_.isInstanceOf[IPartHandler]).map(_.asInstanceOf[IPartHandler].loadFrom(that))
    }
    
    override def clearParts()
    {
        parent.clearParts()
        parent.traitMap.values.filter(_.isInstanceOf[IPartHandler]).map(_.asInstanceOf[IPartHandler].clearParts())
    }
}
