package com.matthewtyler.pe.competition

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.constraints.TournamentConstraints
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.state.pestate.PEState

import scala.annotation.tailrec

/**
  * PECompetition class.
  *
  * Encapsulates a competition between an Evader and pursuer Agent.
  */
case class PECompetition(evader: Agent,
                         pursuer: Agent,
                         tournamentConstraints: TournamentConstraints,
                         withHistory: Boolean = false) extends Logging {

  /**
    * Run competition between Evader and Pursuer.
    *
    * Competition will terminate if capture occurs or pursuer becomes inactive.
    */
  def runCompetition = {

    // Update competition.
    @tailrec
    def update(updates: Int, evaderState: PEState, pursuerState: PEState, history: PECompetitionResult): PECompetitionResult = {

      //Initial simple capture logic.
      //TODO - calculate intercept.
      val distance = (evaderState.position - pursuerState.position).magnitude

      // Capture event has occurred
      if (MathHelper.<=(distance, tournamentConstraints.captureDistance)) {
        debug(s"Pursuer wins! ${pursuerState.agent} has captured ${evaderState.agent} at competition time ${updates}. Evader State ${if (evaderState.isActive) "ACTIVE" else "INACTIVE"}.")
        history.update(evaderState, pursuerState, true, false).publish(withHistory)
      }
      // Survival event has occurred.
      else if (!pursuerState.isActive) {
        debug(s"Evader wins! ${evaderState.agent} survived. Competition time ${updates}. PursuerState ${if (evaderState.isActive) "ACTIVE" else "INACTIVE"}.")
        history.update(evaderState, pursuerState, false, true).publish(withHistory)
      }
      // Keep on truckin
      else {
        update(updates + 1, evader.applyInstruction(evaderState, pursuerState), pursuer.applyInstruction(pursuerState, evaderState), history.update(evaderState, pursuerState))
      }
    }

    // Return list of heats results
    for (_ <- 1 to tournamentConstraints.competitionHeats)
      yield update(0, PEState.initialState(evader, tournamentConstraints.startDistance), PEState.initialState(pursuer, 0.0), PECompetitionResult(Nil, Nil, 0.0, 0.0))
  }
}