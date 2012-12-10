package com.matthewtyler.pe.strategy.pestrategy

import com.matthewtyler.pe.applicable.Applicable
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.state.pestate.PEState

/**
 * AngularBucket class
 */
class AngularBucket (val index : Long,
                     val thetaMin : Double,
                     val thetaMax : Double,
                     val radialBuckets : RadialBucketContainer) extends Applicable with Instruction with Mateable[AngularBucket] {

  // Assert we have a positive sized bucket with the bound in the range 0..2PI
  assert(thetaMax > thetaMin && thetaMin >= 0.0 && MathHelper.<=(thetaMax,MathHelper.TWO_PI))
  
  private val angularBucketString = "AngularBucket\n-------------\nIndex      : %s\nMin Radius : %s\nMax Radius : %s\n%s\n".format(index,thetaMin,thetaMax,radialBuckets)
  
  /**
   * Implement Applicable Trait.
   * 
   * Returns true if the position of the passed PEState
   * lies in the angle subtended by this AngularBucket.
   */
  def applies(myState : PEState) = myState.position.theta >= thetaMin && myState.position.theta < thetaMax
  
  /**
   * Implement Instruction trait.
   */
  def apply(myState : PEState,opponentState : PEState) = radialBuckets.apply(myState, opponentState)
  
  /**
   * Implement Mateable trait
   */
  def mate(partner : AngularBucket) = new AngularBucket(index,thetaMin,thetaMax,radialBuckets mate partner.radialBuckets)
  
  /**
   * String representation of this AngularBucket
   */
  override def toString = angularBucketString
}

