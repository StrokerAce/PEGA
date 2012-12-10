package com.matthewtyler.pe.strategy.pestrategy

import com.matthewtyler.pe.applicable.Applicable
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.tactic.PETactic

/**
 * RadialBucket class.
 */
class RadialBucket(val index : Long,
                   val radiusMin : Int,
                   val radiusMax : Int,
                   val tactic : PETactic,
                   val tournamentConstraints : TournamentConstraints) extends Applicable with Instruction with Mateable[RadialBucket] {
  
  // Assert we have a non zero positively sized bucket.
  assert(radiusMax > radiusMin && radiusMin >= 0)
  
  val size = radiusMax - radiusMin
  
  // toString pattern
  private val radialBucketString = "RadialBucket\n------------\nIndex      : %d\nMin radius : %d\nMax radius : %d\nTactic     : %s\n".format(index,radiusMin,radiusMax,tactic);
  
  /**
   * Implement Applicable trait.
   * 
   * Returns true if state position magnitude (radial  polar component)
   * is bounded by this RadialBucket.
   */
  def applies(s : PEState) = s.position.magnitude >= radiusMin && s.position.magnitude < radiusMax
  
  /**
   * Implement Instruction trait
   */
  def apply(myState : PEState,opponentState : PEState) = tactic.apply(myState, opponentState)
  
  /**
   * Implement Mateable trait.
   * 
   * New Radial will have minRadius of this bucket, size p.size or if mutated 1..p.size
   */
  def mate(partner : RadialBucket) = 
    new RadialBucket(index, radiusMin, if(tournamentConstraints.mutate) radiusMin + RandomFactory.nextIntInRange(partner.size) else radiusMin + partner.size, tactic mate partner.tactic,tournamentConstraints)
  
  /**
   * String representation of this RadialBucket.
   */
  override def toString = radialBucketString
}