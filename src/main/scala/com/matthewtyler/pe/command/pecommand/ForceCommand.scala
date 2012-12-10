package com.matthewtyler.pe.command.pecommand

import com.matthewtyler.pe.command.CommandHelper
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.physics.PhysicsHelper
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState

/**
 * ForceCommand class
 */
class ForceCommand(val forceFactor : Double, 
                   val speedFactor : Double,
                   tournamentConstraints : TournamentConstraints) extends Command with Logging with Mateable[ForceCommand] {
   
  import scala.math._
  
  private val speedCommandString = "Apply %s of maximum force until speed is %s of maximum.".format(forceFactor,speedFactor)

  /**
   * Override equals
   */
  override def equals(other : Any) = other match {
    case that : ForceCommand => MathHelper.doubleEquals(forceFactor,that.forceFactor) && MathHelper.doubleEquals(speedFactor,that.speedFactor)
    case _ => false
  }
  
  /**
   * Implement Command trait
   * 
   * Apply force to accelerate towards target speed.
   * Force limited by remaining energy.
   * Return new state.
   */
  def evaluate(state : PEState,opponentState : PEState = null) = {
    
    if(MathHelper.doubleEquals(state.energy,0.0) && MathHelper.>=(state.velocity.magnitude,0.0)) {
       
      val newVelocity = state.velocity.addMagnitude(-PhysicsHelper.decaySpeed(state))
      
      PEState(state.position + newVelocity,newVelocity,state.agent,state.energy,state.remainingBonus)
    }
    else {
      
      debug("Evaluating command {}. Current speed {}.",this,state.velocity.magnitude)
      val constraints     = state.agent.constraints
      val force           = constraints.maxForce
      val maxSpeed        = constraints.maxSpeed
      val currentSpeed    = state.velocity.magnitude
      val targetSpeed     = maxSpeed * speedFactor
      val targetForce     = constraints.maxForce * forceFactor
      val energyRemaining = state.energy 
      val mass            = constraints.mass
      
      // Required acceleration
      val targetAcceleration = targetSpeed - currentSpeed
      
      val actualAcceleration = PhysicsHelper.actualAcceleration(mass,energyRemaining,force,targetAcceleration)
      
      val newVelocity = state.velocity.addMagnitude(actualAcceleration)
      
      val accelerationEnergy = PhysicsHelper.energyToAccelerate(mass,actualAcceleration)
           
      val frictionEnergy = PhysicsHelper.energyToMaintainSpeed(mass,newVelocity.magnitude,state.energy - accelerationEnergy)
      
      PEState(state.position + newVelocity,newVelocity,state.agent,state.energy - accelerationEnergy - frictionEnergy,state.remainingBonus)
    }
  }
  
  /**
   * Implement Mateable trait.
   */
  def mate(p : ForceCommand) = {
    
    val leftForce = forceFactor
    val rightForce = p.forceFactor
    
    val leftSpeed = speedFactor
    val rightSpeed = p.speedFactor
    
    if(tournamentConstraints.mutate) {
      
      val minForce = min(leftForce,rightForce)
      val maxForce = max(leftForce,rightForce)
      
      val mutatedForce = minForce + (RandomFactory.nextDoubleInRange(maxForce-minForce))
      
      val minSpeed = min(leftSpeed,rightSpeed)
      val maxSpeed = max(leftSpeed,rightSpeed)
      
      val mutatedSpeed = minSpeed + (RandomFactory.nextDoubleInRange(maxSpeed-minSpeed))      
      
      new ForceCommand(mutatedForce,mutatedSpeed,tournamentConstraints)
    }
    else {
      new ForceCommand(RandomFactory.randomPick(leftForce,rightForce),RandomFactory.randomPick(leftSpeed,rightSpeed),tournamentConstraints)
    }
  }
  
  /**
   * String representation of this SpeedCommand.
   */
  override def toString() = speedCommandString 
}

/**
 * ForceCommand Factory.
 */
object ForceCommand {
  
  /**
   * New random SpeedCommand
   */
  def newRandom(tournamentConstraints : TournamentConstraints) = new ForceCommand(RandomFactory.randomGenerator.nextDouble(),RandomFactory.randomGenerator.nextDouble(),tournamentConstraints)
}