package com.matthewtyler.pe.test.command.pecommand

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.command.pecommand.ForceCommand
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.{MathHelper, Vector}
import com.matthewtyler.pe.physics.PhysicsHelper
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.test.constraints.TestConstraints


/**
 * SpeedCommand test suite.
 */
class ForceCommandTestSuite extends Suite with Logging with Matchers {

  import scala.math._
  
  /**
   * Test SpeedCommand newRandom factory method.
   */
  def testNewRandom = {
    
    for(i <- 1 to 100) {
      
      val forceCommand = ForceCommand.newRandom(TestConstraints.tournamentConstraints)
      
      // forceFactor should be in range 0.0..1.0
      forceCommand.forceFactor should (be >= (0.0) and be <= (1.0))
      
      // speedFactor should be in range 0.0..1.0
      forceCommand.speedFactor should (be >= (0.0) and be <= (1.0))
    }
  }
  
  /**
   * Test SpeedCommand mate method.
   */
  def testMate = {
    
    for(i <- 1 to 1000) {
      
      val parent1 = ForceCommand.newRandom(TestConstraints.tournamentConstraints)
      val parent2 = ForceCommand.newRandom(TestConstraints.tournamentConstraints)
      
      val minForceFactor = min(parent1.forceFactor,parent2.forceFactor)
      val maxForceFactor = max(parent1.forceFactor,parent2.forceFactor)
      
      val minSpeedFactor = min(parent1.speedFactor,parent2.speedFactor)
      val maxSpeedFactor = max(parent1.speedFactor,parent2.speedFactor)
            
      val offspring = parent1 mate parent2
 
      offspring.forceFactor should(be >= (minForceFactor) and be <= (maxForceFactor))
      offspring.speedFactor should(be >= (minSpeedFactor) and be <= (maxSpeedFactor))
    }
  }
  
  /**
   * Validate ForceCommand applies expected acceleration.
   */
  private def validateForceCommand(state : PEState,requiredSpeed : Double,command : ForceCommand) : Boolean = {
       
    val requiredAcceleration = min(abs(requiredSpeed - state.velocity.magnitude),abs(state.agent.constraints.maxAcceleration)) * signum(requiredSpeed - state.velocity.magnitude)

    val nextState = command.evaluate(state)
    val nextSpeed = nextState.velocity.magnitude
    val actualAcceleration = nextSpeed - state.velocity.magnitude
    val energyToAccelerate = PhysicsHelper.energyToAccelerate(state.agent.constraints.mass,requiredAcceleration)
    val frictionEnergy = PhysicsHelper.energyToMaintainSpeed(state.agent.constraints.mass,state.velocity.magnitude + actualAcceleration,state.energy - energyToAccelerate)
    val requiredEnergy = energyToAccelerate + frictionEnergy
    val actualEnergy = state.energy - nextState.energy
    
    debug("Current state \n{}",state)
    debug("Required speed {}",requiredSpeed)
    debug("ForceCommand {}",command)
    debug("Required Acceleration {}",requiredAcceleration)
    debug("Max Acceleration {}",state.agent.constraints.maxAcceleration)
    debug("Required Energy {}",requiredEnergy)
    debug("Next state \n{}",nextState)
    debug("Next speed {}",nextSpeed)
    debug("Actual acceleration {}",actualAcceleration)
    debug("Actual energy {}",actualEnergy)
    
    // Validate state update burned the correct amount of energy.
    if(!MathHelper.doubleEquals(requiredEnergy,actualEnergy)) {
      error("Required energy {}, actual energy {}",requiredEnergy,actualEnergy)
      false
    }
    // Accelerated to target.
    else if(MathHelper.doubleEquals(nextSpeed,requiredSpeed)) {
      debug("Accelerated to required speed {}",requiredSpeed)
      true
    }
    else {
      // Acceleration not as expected.
      if(!MathHelper.doubleEquals(actualAcceleration,requiredAcceleration)) {
        error("Actual acceleration {} does not equal required accceleration {}.",actualAcceleration,requiredAcceleration)
        false
      }
      // Not there yet. Keep on truckin'
      else {
        debug("Still {} short of required speed {}",requiredSpeed - state.velocity.magnitude,requiredSpeed)
        validateForceCommand(nextState,requiredSpeed,command)
      }
    }
  }  
  
  /**
   * Test SpeedCommand positive accelerate to target
   */
  private def speedCommandAccelerate(initialSpeed : Double) = {
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    val constraints = TestConstraints.evaderConstraints
    
    for(i <- 1 to 1000) {
            
      // Initial state
      val initialPosition = Vector(0.0,0.0)
      val initialVelocity = Vector.randomVectorConstMagnitude(initialSpeed)
      
      val initialState = PEState(initialPosition,initialVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
      
      initialState.velocity.magnitude should be(initialSpeed +- MathHelper.EPS_DOUBLE)
      
      debug("Validating Force Command")
      val command = ForceCommand.newRandom(TestConstraints.tournamentConstraints)
      
      val targetSpeed = command.speedFactor * constraints.maxSpeed
      
      validateForceCommand(initialState,targetSpeed,command) should be (true)
    }
  }
  
  /**
   * Test ForceCommand positive acceleration.
   */
  def testForceCommandPositive = speedCommandAccelerate(0.0)
  
  /**
   * Test ForceCommand negative acceleration.
   */
  def testForceCommandNegative = speedCommandAccelerate(TestConstraints.evaderConstraints.maxSpeed)
    
}

/**
 * Run the test suite.
 */
object ForceCommandTestSuite extends App {
  (new ForceCommandTestSuite).execute()
}
