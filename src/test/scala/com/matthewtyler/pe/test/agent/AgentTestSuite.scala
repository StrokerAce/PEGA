package com.matthewtyler.pe.test.agent

import org.scalatest.Suite

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.math.Vector
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * Agent test suite
 */
class AgentTestSuite extends Suite {

  /**
   * Test Agent apply method.
   */
  def testApply = {
    
    def evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    def pursuer = Agent.newRandomPursuer(TestConstraints.pursuerConstraints,TestConstraints.tournamentConstraints)
    
    val evaderInitialPosition = Vector.randomVectorConstMagnitude(0.0)
    val evaderInitialVelocity = Vector.randomVectorRandomMagnitude(evader.constraints.maxSpeed)
      
    val pursuerInitialPosition = Vector.randomVectorConstMagnitude(100.0)
    val pursuerInitialVelocity = Vector.randomVectorRandomMagnitude(pursuer.constraints.maxSpeed)
      
    val evaderInitialState = new PEState(evaderInitialPosition,evaderInitialVelocity,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
    val pursuerInitialState = new PEState(pursuerInitialPosition,pursuerInitialVelocity,pursuer,pursuer.constraints.startingEnergy,pursuer.constraints.winBonus)
    
    evader.applyInstruction(evaderInitialState,pursuerInitialState)
  }
}

/**
 * Run the test suite
 */
object AgentTestSuite extends App {
  (new AgentTestSuite).execute()
}