package com.matthewtyler.pe.test.strategy.pestrategy

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.competition.{PECompetition, PECompetitionResult}
import com.matthewtyler.pe.math.Vector
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.pestrategy.PEStrategy
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * StrategyTestSuite
 */
class PEStrategyTestSuite extends Suite with Matchers {

  /**
   * Test apply method
   */
  def testApply = {
    
    for(i <- 1 to 100) {
      
      val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
      val pursuer = Agent.newRandomPursuer(TestConstraints.pursuerConstraints,TestConstraints.tournamentConstraints)
            
      val competitionResults = (PECompetition(evader,pursuer,TestConstraints.tournamentConstraints)).runCompetition
      
      // Validate the number of heats as expected.
      competitionResults.size should equal(TestConstraints.tournamentConstraints.competitionHeats)
      
      // Validate competition results
      for(competitionResult <- competitionResults) {
        
        // Validate result contains identical sized list of states
        competitionResult.evaderStates.size should equal(competitionResult.pursuerStates.size)
        
        // Validate evader states.
        for(evaderState <- competitionResult.evaderStates) {
          evaderState.velocity.magnitude should (be <= evader.constraints.maxSpeed)
          evaderState.energy should (be >= 0.0)
        }
        
        // Validate pursuer states.
        for(pursuerState <- competitionResult.pursuerStates) {
          pursuerState.velocity.magnitude should (be <= pursuer.constraints.maxSpeed)
          pursuerState.velocity.magnitude should (be >= 0.0)
        }
      }
    }
  }
}

/**
 * Run the test suite.
 */
object PEStrategyTestSuite extends App {
  (new PEStrategyTestSuite).execute()
}