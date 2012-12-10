package com.matthewtyler.pe.strategy.tactic

import com.matthewtyler.pe.command.pecommand.{RotationCommand,ForceCommand}
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState

/**
 * PETactic class
 */
class PETactic(val rotationCommand : RotationCommand,val forceCommand : ForceCommand) extends Instruction with Mateable[PETactic] {
  
  private val peTacticString = "PETactic\nRotation Command : %s\n Force Command : %s\n".format(rotationCommand,forceCommand)
  
  /**
   * Implement Instruction trait.
   * Apply instructions to generate new state.
   */
  def apply(myState : PEState,opponentState : PEState) = forceCommand.evaluate(rotationCommand.evaluate(myState,opponentState))
  
  /**
   * Implement Mateable trait.
   */
  def mate(partner : PETactic) = {
    
    // New PETactic will crossover RotationCommand or SpeedCommand at random.
    if(RandomFactory.randomGenerator.nextBoolean()) {
      new PETactic(rotationCommand mate partner.rotationCommand,forceCommand)
    }
    else {
      new PETactic(rotationCommand,forceCommand mate partner.forceCommand)
    }
  }
  
  /**
   * String representation of this PETactic
   */
  override def toString = peTacticString
}

/**
 * PETactic singleton factory.
 */
object PETactic {
  
  /**
   * Create new random PETactic
   */
  def newRandom(tournamentConstraints : TournamentConstraints) = new PETactic(RotationCommand.newRandom(tournamentConstraints),ForceCommand.newRandom(tournamentConstraints))
}


















