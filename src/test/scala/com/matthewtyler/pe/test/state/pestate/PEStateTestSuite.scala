package com.matthewtyler.pe.test.state.pestate

import org.scalatest.Matchers
import org.scalatest.Suite

import scala.annotation.tailrec

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.math.Vector
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * Unit tests for PEState class and helpers.
 */
class PEStateTestSuite extends Suite with Logging with Matchers {
  
  import scala.math._
  
  /**
   * Test PEState next state.
   */
  def testNextState = {
    
    val initialPosition = Vector(0,0)
    val initialVelocity = Vector(0,0)
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    
    val initialState = PEState(initialPosition,initialVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
    
    val maxAcceleration = evader.constraints.maxAcceleration
     
    // Rotate by +Pi radians and increase speed by maxAcceleration
    val nextState1 = initialState.nextState(Pi, maxAcceleration)
       
    // Validate next position.
    nextState1.position.magnitude should equal(maxAcceleration)
    nextState1.position.theta should equal(Pi)
    
    // Validate next velocity
    nextState1.velocity.magnitude should equal(maxAcceleration)
    nextState1.velocity.theta should equal(Pi)
    
    // Rotate by -Pi/2 radians and set increase speed by maxAcceleration / 2.0
    val nextState2 = nextState1.nextState(-(Pi / 2.0),maxAcceleration/2.0)
    
    // Validate next position.
    nextState2.position.magnitude should equal(sqrt(pow(maxAcceleration, 2.0) + pow(1.5 * maxAcceleration, 2.0)))
    nextState2.position.theta should equal(MathHelper.getR(-maxAcceleration, 1.5 * maxAcceleration) )
    
    // Validate next velocity
    nextState2.velocity.magnitude should equal(1.5 * maxAcceleration)
    nextState2.velocity.theta should equal(Pi / 2.0)
  }

  /**
   * Test PEState toFrameOfReference
   * 
   * Tests conversion of opponent state into observer states state of reference.
   */
  def testToFramOfReference = {
    
    // Define Observer state
    val observerPosition = Vector(5.0,5.0)
    val observerVelocity = Vector(-100.0,-100.0)
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    
    val observerState = PEState(observerPosition,observerVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
    
    // Define opponent state
    val opponentPosition = Vector(0.0,0.0)
    val opponentVelocity = Vector(200.0,200.0)
    
    val pursuer = Agent.newRandomPursuer(TestConstraints.pursuerConstraints,TestConstraints.tournamentConstraints)
    
    val opponentState = PEState(opponentPosition,opponentVelocity,pursuer,pursuer.constraints.startingEnergy,evader.constraints.winBonus)
    
    val translatedObserverState = PEState.toFrameOfReference(observerState,opponentState)
    
    // New opponent position should be translated by - observer position and rotated by - observerVelocity.theta
    val expectedOpponentPosition = (opponentPosition - observerPosition) rotate -observerVelocity.theta
    
    translatedObserverState.position should equal(expectedOpponentPosition)
       
    // New opponent velocity should be rotated by -observerOrientation.theta
    val expectedOpponentVelocity = opponentVelocity rotate -observerVelocity.theta
    
    translatedObserverState.velocity should equal(expectedOpponentVelocity)
  }
  
  /**
   * Test PEState expand method.
   */
  def testExpand = {
    
    val agent = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
   
    val p1 = Vector(0.0,0.0)
    val v1 = Vector(0.0,0.0)
    val e1 = 1000.0
    
    val s1 = PEState(p1,v1,agent,e1,agent.constraints.winBonus)
    
    val p2 = Vector(100.0,100.0)
    val v2 = Vector(100.0,100.0)
    val e2 = 500.0
    
    val s2 = PEState(p2,v2,agent,e2,agent.constraints.winBonus)
    
    val states = List(s1,s2)
    
    val expansionRatio = 37
    
    val expandedStates = PEState.expandStates(states,expansionRatio)
    
    val deltaTime = 1.0 / expansionRatio
    val deltaVelocity = v2.scale(deltaTime)
    val deltaEnergy = (e1 - e2)  * deltaTime
    
    // Validate states.
    @tailrec def validateStates(validationStates : List[PEState]): Unit = validationStates match {
    
      case List() =>
      // Final state should equal s2
      case h :: Nil => h should equal(s2)
      case h :: t => {
        
        val initialState = h
        val finalState = t.head
      
        // Validate energy difference
        MathHelper.doubleEquals(initialState.energy - finalState.energy,deltaEnergy) should be(true)
        
        // Validate position difference
        initialState.position + deltaVelocity should equal(finalState.position)
        
        validateStates(t)
      }
    }
    
    validateStates(expandedStates)
  }
}

/**
 * Run the test suite.
 */
object PEStateTestSuite extends App {
  (new PEStateTestSuite).execute()
}
