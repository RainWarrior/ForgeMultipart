package codechicken.multipart

import codechicken.core.vec.Cuboid6
import cpw.mods.fml.relauncher.SideOnly
import cpw.mods.fml.relauncher.Side
import net.minecraft.util.Icon
import net.minecraft.util.MovingObjectPosition
import net.minecraft.client.particle.EffectRenderer
import codechicken.core.vec.Vector3
import codechicken.core.raytracer.ExtendedMOP
import codechicken.core.render.EntityDigIconFX

object IconHitEffects
{
    def addHitEffects(part:JIconHitEffects, hit:MovingObjectPosition, effectRenderer:EffectRenderer)
    {
        EntityDigIconFX.addBlockHitEffects(part.tile.worldObj, 
                part.getBounds.copy.add(Vector3.fromTileEntity(part.tile)), hit.sideHit, 
                part.getBreakingIcon(ExtendedMOP.getData[(_, _)](hit)._2, hit.sideHit), effectRenderer)
    }
    
    def addDestroyEffects(part:JIconHitEffects, effectRenderer:EffectRenderer)
    {
        addDestroyEffects(part, effectRenderer, true)
    }
    
    def addDestroyEffects(part:JIconHitEffects, effectRenderer:EffectRenderer, scaleDensity:Boolean)
    {
        val icons = new Array[Icon](6)
        for(i <- 0 until 6)
            icons(i) = part.getBrokenIcon(i)
        val bounds = 
            if(scaleDensity) part.getBounds.copy
            else Cuboid6.full.copy
        EntityDigIconFX.addBlockDestroyEffects(part.tile.worldObj, 
                bounds.add(Vector3.fromTileEntity(part.tile)), icons, effectRenderer)
    }
}

trait JIconHitEffects extends TMultiPart
{
    def getBounds:Cuboid6
    
    @SideOnly(Side.CLIENT)
    def getBreakingIcon(subPart:Any, side:Int) = getBrokenIcon(side)
    
    @SideOnly(Side.CLIENT)
    def getBrokenIcon(side:Int):Icon
}

trait TIconHitEffects extends JIconHitEffects
{
    @SideOnly(Side.CLIENT)
    override def addHitEffects(hit:MovingObjectPosition, effectRenderer:EffectRenderer)
    {
        IconHitEffects.addHitEffects(this, hit, effectRenderer)
    }
    
    @SideOnly(Side.CLIENT)
    override def addDestroyEffects(effectRenderer:EffectRenderer)
    {
        IconHitEffects.addDestroyEffects(this, effectRenderer)
    }
}