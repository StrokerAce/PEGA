package com.matthewtyler.pe.competition

import scala.annotation.tailrec

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.{MathHelper, Vector}
import com.matthewtyler.pe.state.pestate.PEState

/**
 * PECompetition class.
 * 
 * Encapsulates a competition between an Evader and pursuer Agent.
 */
case class PECompetition(private val evader                : Agent,
                         private val pursuer               : Agent,
                         private val tournamentConstraints : TournamentConstraints,
                         private val withHistory           : Boolean = false) extends Logging {

  /**
   * Run competition between Evader and Pursuer.
   * 
   * Competition will terminate if capture occurs or pursuer becomes inactive.
   */
  def runCompetition() = {
        
    // Update competition.
    @tailrec def update(updates : Int,evaderState : PEState,pursuerState : PEState,history : PECompetitionResult) : PECompetitionResult = {
      
      //debug("Competion timeslice {}", updates)
      //debug("Evader is{} active", if(evaderState.isActive) "" else " NOT")
      //debug("Pursuer is{} active", if(pursuerState.isActive) "" else " NOT")
      //debug("\nEvader State:\n{}", evaderState)
      //debug("\nPursuer State:\n{}", pursuerState)
      
      //Initial simple capture logic.
      //TODO - calculate intercept.
      val distance = (evaderState.position - pursuerState.position).magnitude
      
      //debug("Distance between agents {}", distance)
       
      // Assert constraints have not been violated.
      //if(history.evaderStates != List()) {
      //  val evaderHead = history.evaderStates.head    
      //  if(!MathHelper.<=(evaderState.velocity.magnitude,evader.constraints.maxSpeed)) {
      //    throw new RuntimeException("Evader max speed exceeded actual %s limit %s".format(evaderState.velocity.magnitude,evader.constraints.maxSpeed))
      //  }
      //}
      
      //if(history.pursuerStates != List()) { 
      //  val pursuerHead = history.pursuerStates.head    
      //  if(!MathHelper.<=(pursuerState.velocity.magnitude,pursuer.constraints.maxSpeed)) {
      //    throw new RuntimeException("Pursuer max speed exceeded actual %s limit %s".format(pursuerState.velocity.magnitude,pursuer.constraints.maxSpeed))
      //  }
      //}
      
      // Capture event has occurred
      if(MathHelper.<=(distance, tournamentConstraints.captureDistance)) {
        debug("Pursuer wins! {} has captured {} at competition time {}. Evader State {}.",pursuerState.agent,evaderState.agent, updates,if(evaderState.isActive) "ACTIVE" else "INACTIVE")
        history.update(evaderState,pursuerState,true,false).publish(withHistory)
      }
      // Survival event has occurred.
      else if (!pursuerState.isActive) {
        debug("Evader wins! {} survived. Competition time {}. PursuerState {}", evaderState.agent, updates, if(pursuerState.isActive) "ACTIVE" else "INACTIVE")
        history.update(evaderState,pursuerState,false,true).publish(withHistory)
      }
      // Keep on truckin
      else {
        update(updates + 1,evader.apply(evaderState, pursuerState), pursuer.apply(pursuerState, evaderState), history.update(evaderState, pursuerState))
      }
    }
    
    // Return list of heats results
    for(heat <- 1 to tournamentConstraints.competitionHeats) 
      yield update(0,PEState.initialState(evader,tournamentConstraints.startDistance),PEState.initialState(pursuer,0.0),PECompetitionResult(Nil,Nil,0.0,0.0))
  }
}