package com.matthewtyler.pe.test.strategy.tactic

import org.scalatest.Suite
import org.scalatest.Matchers

import scala.annotation.tailrec

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.command.pecommand.{ForceCommand,RotationCommand}
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.{MathHelper,Vector}
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.tactic.PETactic
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * PETactic test suite
 */
class PETacticTestSuite extends Suite with Logging with Matchers {

  import scala.math._
  
  /**
   * Test apply method.
   */
  def testApply = {
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)   
    val constraints = TestConstraints.evaderConstraints
    
    val initialPosition = Vector(0.0,0.0)
    val initialVelocity = Vector(0.0,0.0)
    
    for(i <- 1 to 1000) {
      
      val initialState = PEState(initialPosition,initialVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
      
      val opponentInitialPosition = Vector.randomVectorRandomMagnitude(1000.0)
      val opponentInitialVelocity = Vector(0.0,0.0)
      
      val opponentState = PEState(opponentInitialPosition,opponentInitialVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
       
      val rotationCommand = RotationCommand.newRandom(TestConstraints.tournamentConstraints)
      val forceCommand = ForceCommand.newRandom(TestConstraints.tournamentConstraints)
    
      val tactic = new PETactic(rotationCommand,forceCommand)
     
      // Validate tactic
      @tailrec def validate(state : PEState,repeat : Int) : Boolean = {
                
        // Convert opponent state to correctFrameOfReference
        val preFrameOfReferenceState = PEState.toFrameOfReference(state,opponentState)
          
        val afterRotationState = rotationCommand.evaluate(state,preFrameOfReferenceState)
        
        val afterForceState = forceCommand.evaluate(state,preFrameOfReferenceState)
       
        val afterTacticState = tactic.apply(state,preFrameOfReferenceState)
        
        val postFrameOfReferenceState = PEState.toFrameOfReference(afterTacticState,opponentState)
        
        val currentSpeed = afterTacticState.velocity.magnitude
        val currentHeading = afterTacticState.velocity.theta
        
        val opponentPreHeading = MathHelper.normaliseAngle(state.velocity.theta + preFrameOfReferenceState.position.theta)
                
        val targetHeading = MathHelper.normaliseAngle(opponentPreHeading + rotationCommand.headingOffset) 
        val targetSpeed = forceCommand.speedFactor * evader.constraints.maxSpeed
         
        debug("Current speed {} target speed {}, obtained {}",currentSpeed,targetSpeed,MathHelper.doubleEquals(currentSpeed,targetSpeed))
        debug("Current heading {}, target heading {}, obtained {}",currentHeading,targetHeading,MathHelper.doubleEquals(currentHeading,targetHeading))
        debug("Current energy {}",afterTacticState.energy)
        
        if(afterTacticState.isActive && !MathHelper.doubleEquals(afterRotationState.velocity.theta,afterTacticState.velocity.theta)) {
          debug("After rotation heading {} does not equal after tactic heading {}.",afterRotationState.velocity.theta,afterTacticState.velocity.theta)
          false
        }
        else if(afterTacticState.isActive && !MathHelper.doubleEquals(afterForceState.velocity.magnitude,afterTacticState.velocity.magnitude)) {
          debug("After force magnitude {} does not equal after tactic magnitude {}.",afterForceState.velocity.magnitude,afterTacticState.velocity.magnitude)
          false
        }
        else if(MathHelper.doubleEquals(targetHeading,currentHeading) && MathHelper.doubleEquals(targetSpeed,currentSpeed)) {
          debug("Required heading {} and speed {} obtained.",targetHeading,targetSpeed)
          true
        }
        else if(!afterTacticState.isActive) {
          debug("Inactive. Target speed {}",targetSpeed)
          true
        }
        else {
          validate(afterTacticState,repeat-1)
        }
      }
      
      validate(initialState,10) should be(true)
    }
  }
  
  /**
   * Test PETactic mate method.
   */
  def testMate = {
    
    for(i <- 1 to 1000) {
      
      val parent1 = PETactic.newRandom(TestConstraints.tournamentConstraints)
      val parent2 = PETactic.newRandom(TestConstraints.tournamentConstraints)

      val offspring = parent1 mate parent2

      val p1RC = parent1.rotationCommand
      val p2RC = parent2.rotationCommand
      
      val maxRCHeadingOffset = max(p1RC.headingOffset,p2RC.headingOffset)
      val minRCHeadingOffset = min(p1RC.headingOffset,p2RC.headingOffset)

      val p1SC = parent1.forceCommand
      val p2SC = parent2.forceCommand
      
      val maxSCForceFactor = max(p1SC.forceFactor,p2SC.forceFactor)
      val minSCForceFactor = min(p1SC.forceFactor,p2SC.forceFactor)
      
      val maxSCSpeedFactor = max(p1SC.speedFactor,p2SC.speedFactor)
      val minSCSpeedFactor = min(p1SC.speedFactor,p2SC.speedFactor)
      
      val oRC = offspring.rotationCommand
      val oSC = offspring.forceCommand

      oRC.direction should (equal(p1RC.direction) or equal(p2RC.direction))
      oRC.headingOffset should(be >= (minRCHeadingOffset) or be <= (maxRCHeadingOffset))
    
      oSC.forceFactor should(be >=(minSCForceFactor) and be <=(maxSCForceFactor))
      oSC.speedFactor should(be >=(minSCSpeedFactor) and be <=(maxSCSpeedFactor))
    }
  } 
 }

/**
 * Run the test suite.
 */
object PETacticTestSuite extends App {
  (new PETacticTestSuite).execute()
}