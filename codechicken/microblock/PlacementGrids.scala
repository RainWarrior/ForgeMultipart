package codechicken.microblock

import codechicken.core.vec.Vector3
import org.lwjgl.opengl.GL11
import codechicken.core.vec.BlockCoord
import codechicken.core.vec.Rotation

trait PlacementGrid
{
    def getHitSlot(vhit:Vector3, side:Int):Int
    
    def render(hit:Vector3, side:Int)
    
    def glTransformFace(hit:Vector3, side:Int)
    {
        val pos = new BlockCoord(hit)
        GL11.glPushMatrix()
        GL11.glTranslated(pos.x+0.5, pos.y+0.5, pos.z+0.5)
        Rotation.sideRotations(side).glApply()
        val rhit = new Vector3(pos.x+0.5, pos.y+0.5, pos.z+0.5).subtract(hit).rotate(Rotation.sideQuatsR(side^1))
        GL11.glTranslated(0, rhit.y-0.002, 0)
    }
}

object FacePlacementGrid extends PlacementGrid
{
    def render(hit:Vector3, side:Int)
    {
        glTransformFace(hit, side)
        
        GL11.glLineWidth(2)
        GL11.glColor4f(0, 0, 0, 1)
        GL11.glBegin(GL11.GL_LINES)
            GL11.glVertex3d(-0.5, 0,-0.5)
            GL11.glVertex3d(-0.5, 0, 0.5)
            
            GL11.glVertex3d(-0.5, 0, 0.5)
            GL11.glVertex3d( 0.5, 0, 0.5)
            
            GL11.glVertex3d( 0.5, 0, 0.5)
            GL11.glVertex3d( 0.5, 0,-0.5)
            
            GL11.glVertex3d( 0.5, 0,-0.5)
            GL11.glVertex3d(-0.5, 0,-0.5)
            
            GL11.glVertex3d(0.5, 0, 0.5)
            GL11.glVertex3d(0.25, 0, 0.25)
            
            GL11.glVertex3d(-0.5, 0, 0.5)
            GL11.glVertex3d(-0.25, 0, 0.25)
            
            GL11.glVertex3d(0.5, 0, -0.5)
            GL11.glVertex3d(0.25, 0, -0.25)
            
            GL11.glVertex3d(-0.5, 0, -0.5)
            GL11.glVertex3d(-0.25, 0, -0.25)
            
            GL11.glVertex3d(-0.25, 0,-0.25)
            GL11.glVertex3d(-0.25, 0, 0.25)
            
            GL11.glVertex3d(-0.25, 0, 0.25)
            GL11.glVertex3d( 0.25, 0, 0.25)
            
            GL11.glVertex3d( 0.25, 0, 0.25)
            GL11.glVertex3d( 0.25, 0,-0.25)
            
            GL11.glVertex3d( 0.25, 0,-0.25)
            GL11.glVertex3d(-0.25, 0,-0.25)
        GL11.glEnd();
        GL11.glPopMatrix()
    }
    
    def getHitSlot(vhit:Vector3, side:Int):Int = 
    {
        val s1 = (side+2)%6
        val s2 = (side+4)%6
        val u = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s1))
        val v = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s2))
        
        if(Math.abs(u) < 4/16D && Math.abs(v) < 4/16D)
            return side^1
        if(Math.abs(u) > Math.abs(v))
            return if(u > 0) s1 else s1^1
        else
            return if(v > 0) s2 else s2^1
    }
}

object CornerPlacementGrid extends PlacementGrid
{
    def render(hit:Vector3, side:Int)
    {
        glTransformFace(hit, side)
        GL11.glLineWidth(2)
        GL11.glColor4f(0, 0, 0, 1)
        GL11.glBegin(GL11.GL_LINES)
        
            GL11.glVertex3d(-0.5, 0,-0.5)
            GL11.glVertex3d(-0.5, 0, 0.5)
            
            GL11.glVertex3d(-0.5, 0, 0.5)
            GL11.glVertex3d( 0.5, 0, 0.5)
            
            GL11.glVertex3d( 0.5, 0, 0.5)
            GL11.glVertex3d( 0.5, 0,-0.5)
            
            GL11.glVertex3d( 0.5, 0,-0.5)
            GL11.glVertex3d(-0.5, 0,-0.5)
            
            GL11.glVertex3d(0, 0,-0.5)
            GL11.glVertex3d(0, 0, 0.5)
            
            GL11.glVertex3d(-0.5, 0, 0)
            GL11.glVertex3d( 0.5, 0, 0)
        
        GL11.glEnd();
        GL11.glPopMatrix()
    }
    
    def getHitSlot(vhit:Vector3, side:Int):Int = 
    {
        val s1 = ((side&6)+3)%6
        val s2 = ((side&6)+5)%6
        val u = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s1))
        val v = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s2))
        
        val bu = if(u >= 0) 1 else 0
        val bv = if(v >= 0) 1 else 0
        val bw = (side&1)^1
        
        return 7+(
                bw<<(side>>1)|
                bu<<(s1>>1)|
                bv<<(s2>>1))
    }
}

object EdgePlacementGrid extends PlacementGrid
{
    def render(hit:Vector3, side:Int) = FacePlacementGrid.render(hit, side)
    
    def getHitSlot(vhit:Vector3, side:Int):Int =
    {
        val s1 = (side+2)%6
        val s2 = (side+4)%6
        val u = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s1))
        val v = vhit.copy().add(-0.5, -0.5, -0.5).scalarProject(Rotation.axes(s2))
        
        if(Math.abs(u) < 4/16D && Math.abs(v) < 4/16D)
            return -1
        var b = side&1
        if(Math.abs(u) > Math.abs(v))
        {
            if(u < 0) b^=1
            return 15+((s2&6)<<1 | b<<1 | side&1^1)
        }
        else
        {
            if(v < 0) b^=1
            return 15+((s1&6)<<1 | (side&1^1)<<1 | b)
        }
    }
}