package com.matthewtyler.pe.physics

import com.matthewtyler.pe.constraints.AgentConstraints
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.state.pestate.PEState

/**
 * PhysicsHelper object
 * 
 * Simple methods which take liberties with the laws of Physics.
 */
object PhysicsHelper extends Logging {

  import scala.math._
  
  // TODO : Configure from configuration.
  val decayCoefficient = 0.5
  
  /**
   * Returns the actual acceleration achievable given a max force and energy
   */
  def actualAcceleration(mass : Double,energy : Double, force : Double,requiredAcceleration : Double) =
    signum(requiredAcceleration) * min(force/mass,min(abs(requiredAcceleration),accelerationForEnergy(mass,energy)))
  /**
   * Change in velocity possible for given mass and amount of energy.
   * Assume Acceleration == dv over single timestep.
   */
  def accelerationForEnergy(mass : Double,Energy : Double) = sqrt((2.0 * Energy)/mass) 

  /**
   * Calculate the amount speed will decay due to friction per timestep in the absence of all other forces.
   */
  def decaySpeed(state : PEState) = min(state.velocity.magnitude, state.agent.constraints.maxAcceleration * decayCoefficient)
    
  /**
   * Calculate the energy to accelerate to a new speed.
   * Assume Acceleration == dv over single timestep.
   */
  def energyToAccelerate(mass : Double,acceleration : Double) = 0.5 * mass * acceleration * acceleration
  
  /**
   * Calculate the energy to maintain speed
   */
  def energyToMaintainSpeed(mass : Double,speed : Double,energy : Double) = min((mass * decayCoefficient * speed),energy) 
        
  /**
   * Calculate the max rotation(percentage) for a given speed(percentage) 
   */
  def maxRotationForSpeed(currentSpeed : Double,constraints : AgentConstraints) = {
    
    assert(MathHelper.<=(currentSpeed,constraints.maxSpeed) && MathHelper.>=(currentSpeed,0.0))
    
    if(MathHelper.doubleEquals(currentSpeed,0.0)) 0.0
    else {
      ((((constraints.maxSpeed - currentSpeed)/constraints.maxSpeed) * (1 - constraints.maxAngularVelocityAtMaxSpeed)) + constraints.maxAngularVelocityAtMaxSpeed) * constraints.maxAngularVelocity
    }
  }
}