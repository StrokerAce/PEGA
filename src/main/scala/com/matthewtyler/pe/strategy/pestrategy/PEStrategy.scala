package com.matthewtyler.pe.strategy.pestrategy

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.state.pestate.PEState

/**
 * PEStrategy class.
 */
class PEStrategy private (val angularBucketContainer : AngularBucketContainer) extends Instruction  with Mateable[PEStrategy] {
  /**
   * Implement Instruction trait.
   */
  def applyInstruction(myState : PEState,opponentState : PEState) = angularBucketContainer.applyInstruction(myState, opponentState)
  
  /**
   * Implement Mateable trait.
   */
  def mate(partner : PEStrategy) = new PEStrategy(angularBucketContainer mate partner.angularBucketContainer)
}

/**
 * PEStrategy factory singleton.
 */
object PEStrategy {
    
  /**
   * Create new random PEStrategy
   */
  def newRandom(agentConstraints : AgentConstraints,tournamentConstraints : TournamentConstraints) = 
    new PEStrategy(AngularBucketContainer.newRandom(agentConstraints,tournamentConstraints))  
}


