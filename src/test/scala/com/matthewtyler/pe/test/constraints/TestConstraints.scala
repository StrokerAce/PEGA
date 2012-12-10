package com.matthewtyler.pe.test.constraints

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.state.pestate.PEState

/**
 * Constraints factory for testing
 */
object TestConstraints extends Logging {
   
  val evaderConstraints = AgentConstraints("Evader",8,10,500,100.0,100.0,0.25,0.4,10.0,10000.0,1000.0,0.0,1.0,false,(deltaDistance : Double) => deltaDistance > 0.0)
  
  val pursuerConstraints = AgentConstraints("Pursuer",8,10,500,120.0,120.0,0.2,0.3,12.0,10000.0,1000.0,2.0,1.0,false,(deltaDistance : Double) => deltaDistance < 0.0)

  val tournamentConstraints = TournamentConstraints(50,10,1,100.0,5.0,0.01,2,4,12345678L)
}