package com.matthewtyler.pe.test.command.pecommand

import org.scalatest.Suite
import org.scalatest.matchers.MustMatchers

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.command.pecommand.RotationCommand
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.{MathHelper, Vector}
import com.matthewtyler.pe.physics.PhysicsHelper
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * RotationCommand test suite.
 */
class RotationCommandTestSuite extends Suite with Logging with MustMatchers {
  
  import scala.math._
  
  /**
   * Test RotationCommand newRandom factory method
   */
  def testNewRandom = {
    
    for(i <- 1 to 1000) {
     
      val rotationCommand = RotationCommand.newRandom(TestConstraints.tournamentConstraints)
           
      // Direction must be -1.0 or 1.0.
      // target must be 0.0..2Pi
      rotationCommand.direction must (equal(-1.0) or equal(1.0))
      rotationCommand.headingOffset must(be >= (0.0) or be <= (MathHelper.TWO_PI))
    }
  }
  
  /**
   * Test RotationCommand mate method.
   */
  def testMate = {
    
    for(i <- 1 to 1000) {
      
      val parent1 = RotationCommand.newRandom(TestConstraints.tournamentConstraints)
      val parent2 = RotationCommand.newRandom(TestConstraints.tournamentConstraints)
      
      val minTarget = min(parent1.headingOffset,parent2.headingOffset)
      val maxTarget = max(parent1.headingOffset,parent2.headingOffset)
      
      val offspring = parent1 mate parent2
           
      offspring.direction must((equal(parent1.direction) or equal(parent2.direction)) and (equal(-1.0) or equal(1.0)))
      offspring.headingOffset must(be >=(minTarget) and be <=(maxTarget))
    }
  }
  
  /**
   * Test evaluation of RotationCommand at zero speed.
   * Rotation is restricted at zero speed and next state 
   * should be identical to initial state. 
   */
  def testRotationAtZeroSpeed = {
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    val constraints = TestConstraints.evaderConstraints
    
    val initialPosition = Vector(0.0,0.0)
    val initialVelocity = Vector(0.0,0.0)
    
    val initialState = PEState(initialPosition,initialVelocity,evader,constraints.startingEnergy,evader.constraints.winBonus)
        
    for(i <- 1 to 1000) {
    
      val opponentPosition = Vector.randomVectorConstMagnitude(constraints.maxSpeed)
      val opponentVelocity = Vector(0.0,0.0)
      
      val opponentState = PEState(opponentPosition,opponentVelocity,evader,constraints.startingEnergy,evader.constraints.winBonus)
      
      val rotationCommand = RotationCommand.newRandom(TestConstraints.tournamentConstraints)
      
      val nextState = rotationCommand.evaluate(initialState,opponentState)
      
      initialState must equal(nextState)
    }   
  }
  
  /**
   * 
   */
  def testRotationCommandAtSpeed = {
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    val constraints = TestConstraints.evaderConstraints
    
    for(i <- 1 to 1000) {
      
      val initialPosition = Vector(0.0,0.0)
      val initialVelocity = Vector.randomVectorConstMagnitude(RandomFactory.nextDoubleInRange(constraints.maxSpeed))

      val opponentPosition = Vector.randomVectorConstMagnitude(constraints.maxSpeed)
      val opponentVelocity = Vector(0.0,0.0)
      
      val initialState = PEState(initialPosition,initialVelocity,evader,constraints.startingEnergy,evader.constraints.winBonus)
      val opponentState = PEState(opponentPosition,opponentVelocity,evader,constraints.startingEnergy,evader.constraints.winBonus)
      
      val command= RotationCommand.newRandom(TestConstraints.tournamentConstraints)
      
      // Speed should not change so maximumAngularAcceleration 
      // will be constant for this test.
      val maxRotation = PhysicsHelper.maxRotationForSpeed(initialVelocity.magnitude,constraints)
      val direction = command.direction
         
      // Validate Rotation command correctly calculates next state.
      def validate(state : PEState) : Boolean = {
        
        // Convert opponent state to correctFrameOfReference
        val frameOfReferenceState = PEState.toFrameOfReference(state,opponentState)
        
        val opponentHeading = MathHelper.normaliseAngle(state.velocity.theta + frameOfReferenceState.position.theta)
        
        val targetHeading = MathHelper.normaliseAngle(opponentHeading + command.headingOffset) 
        
        debug("Current heading {}, Opponent offset {}, target {}",state.velocity.theta,frameOfReferenceState.position.theta,targetHeading)
        
        val nextState = command.evaluate(state,frameOfReferenceState)
        
        if(state.position != nextState.position) {
          error("RotationCommand must not modify position.")
          return false
        }
        
        if(!MathHelper.doubleEquals(state.velocity.magnitude,nextState.velocity.magnitude)) {
          error("RotationCommand must not modify speed.")
          return false
        }
        
        if(!MathHelper.doubleEquals(state.energy,nextState.energy)) {
          error("RotationCommand must not modify energy.")
          return false
        }
        
        val preHeading = state.velocity.theta
        val postHeading = nextState.velocity.theta
        
        val requiredRotation = min(abs(MathHelper.angleBetween(preHeading,targetHeading,direction)),maxRotation) * direction
       
        val actualRotation = MathHelper.angleBetween(preHeading,postHeading,direction) * direction      
        
        if(!MathHelper.doubleEquals(requiredRotation,actualRotation)) {
          error("{} did not rotate by required {}, actual {}, pre {}, post {}, target {}",command,requiredRotation,actualRotation,preHeading,postHeading,targetHeading)
          false
        } 
        else {
          if(abs(requiredRotation) < maxRotation) {
            if(MathHelper.doubleEquals(targetHeading,postHeading)) {
              debug("{} rotated to required heading {}",command,postHeading)
              true
            }
            else {
              error("{} did not rotate to required heading {}",command,targetHeading)
              false 
            }
          }
          else {
            debug("{}, maxRotation {} rotated from {} to {}. Target {}",command,maxRotation,preHeading,postHeading,targetHeading)
            validate(nextState)
          }
        }
      }
      
      validate(initialState) must be(true)
    }
  }
} 
   
/**
 * Run the test suite.
 */
object RotationCommandTestSuite extends App {
  (new RotationCommandTestSuite).execute()
}