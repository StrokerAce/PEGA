package com.matthewtyler.pe.command.pecommand

import com.matthewtyler.pe.command.CommandHelper
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.physics.PhysicsHelper
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState

/**
 * RotationCommand
 */
class RotationCommand(val direction : Int,
                      val headingOffset : Double,
                      tournamentConstraints : TournamentConstraints) extends Command with Logging with Mateable[RotationCommand] {
    
  import scala.math._
  
  private val rotationCommandString = "Rotate in %s direction until orientation is opponent heading + %s.".format(CommandHelper.directionToString(direction),headingOffset)
  
  /**
   * Override equals.
   */
  override def equals(other : Any) = other match {
    case that : RotationCommand => MathHelper.doubleEquals(direction,that.direction) && MathHelper.doubleEquals(headingOffset,that.headingOffset)
    case _ => false
  }
  
  /**
   * Implement Command trait
   * 
   * Rotate in the specified direction towards opponent heading + target angle not exceeding constraints.maxAngularVelocity
   * 
   */
  def evaluate(state : PEState,opponentState : PEState) = {
    
    if(!MathHelper.doubleEquals(state.energy,0.0)) {
 
      val opponentHeading = MathHelper.normaliseAngle(state.velocity.theta + opponentState.position.theta)
      val targetHeading = MathHelper.normaliseAngle(opponentHeading + headingOffset)
      
      debug("Evaluating command {}. Current heading {}, Opponent heading {}, Target heading",this,state.velocity.theta,opponentHeading,targetHeading)
      val constraints = state.agent.constraints
      
      val velocity = state.velocity
      
      // max rotation is speed dependent.
      val maxRotation = PhysicsHelper.maxRotationForSpeed(state.velocity.magnitude,constraints)
      
      if(MathHelper.doubleEquals(velocity.theta,targetHeading) ) {
        debug("Already heading in target direction {}. No action required.", targetHeading)
        state
      }
      else {
        
        // translate so target is at zero degrees.
        // This makes calculation a but easier.
        val translatedTarget = 0.0
        val translatedTheta = MathHelper.normaliseAngle(velocity.theta - targetHeading)
        
        if(direction == 1) {
          val rotation = min(maxRotation,MathHelper.TWO_PI - translatedTheta)
          debug("Rotating {}", rotation)
          PEState(state.position,state.velocity.rotate(rotation),state.agent,state.energy,state.remainingBonus)
        }
        else {
          
          // Rotate clockwise.
          val rotation = -min(maxRotation,translatedTheta)
          debug("Rotating {}", rotation)
          PEState(state.position,state.velocity.rotate(rotation),state.agent,state.energy,state.remainingBonus)
        }       
      }
    }
    // We have run out of energy. Do Nothing.
    else {
      state
    }
  }
  
  /**
   * Implement Mateable trait.
   */
  def mate(p : RotationCommand) = {
        
    val left = headingOffset
    val right = p.headingOffset

    if(tournamentConstraints.mutate) {
      // Direction mutated
      val mutatedDirection = min(left,right) + RandomFactory.nextDoubleInRange(max(left,right) - min(left,right))
      
      new RotationCommand(RandomFactory.randomPick(direction,p.direction), mutatedDirection,tournamentConstraints)
    }
    else {
      new RotationCommand(RandomFactory.randomPick(direction,p.direction), RandomFactory.randomPick(left, right),tournamentConstraints)
    }
  }
  
  /**
   * String representation of this RotationCommand
   */
  override def toString = rotationCommandString
}

/**
 * RotationCommand factory singleton
 */
object RotationCommand extends {
  
  /**
   * New random RotationCommand
   */
  def newRandom(tournamentConstraints : TournamentConstraints) = 
    new RotationCommand(RandomFactory.randomPick(CommandHelper.directionNegative,CommandHelper.directionPositive),
                        MathHelper.TWO_PI * RandomFactory.randomGenerator.nextDouble(),
                        tournamentConstraints)
}