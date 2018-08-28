package com.matthewtyler.pe.competition

import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.state.pestate.PEState

import scala.annotation.tailrec

/**
  * Case class to store a competition result.
  */
case class PECompetitionResult(evaderStates: List[PEState],
                               pursuerStates: List[PEState],
                               evaderScore: Double,
                               pursuerScore: Double,
                               survivalEvents: Int = 0,
                               captureEvents: Int = 0) extends Logging {

  import scala.math._

  // Agent results.
  lazy val evaderResult = getEvaderResult
  lazy val pursuerResult = getPursuerResult

  /**
    * Get max x and y value in competition.
    */
  lazy val maxXY = {

    // Compare this PEState with maxXY so far and return updated max values.
    def getMax(agentState: PEState, maxXY: (Double, Double)) = maxXY match {
      case (maxX, maxY) => max(abs(agentState.position.i), maxX) -> max(abs(agentState.position.j), maxY)
    }

    // Calculate max X, Y values for complete list of evader, pursuer states.
    @tailrec
    def limits(evaders: List[PEState], pursuers: List[PEState], maxXY: (Double, Double)): (Double, Double) = (evaders, pursuers) match {
      case (Nil, Nil) => maxXY
      case (evadersHead :: evadersTail, pursuersHead :: pursuersTail) => limits(evadersTail, pursuersTail, getMax(pursuersHead, getMax(evadersHead, maxXY)))
      case _ => throw new IllegalStateException("Pursuer/Evader states size mismatch.")
    }

    limits(evaderStates, pursuerStates, (0.0, 0.0))
  }

  /**
    * Get Evader PEAgentResult
    */
  private def getEvaderResult = {

    if (evaderStates.isEmpty)
      throw new IllegalStateException("evaderStates list empty. Cannot get Evader result.")

    PEAgentResult(evaderStates.head.agent.toString, evaderScore, survivalEvents)
  }

  /**
    * Get Pursuer PEAgentResult
    */
  private def getPursuerResult = {

    if (pursuerStates.isEmpty)
      throw new IllegalStateException("pursuerStates list empty. Cannot get Pursuer result.")

    PEAgentResult(pursuerStates.head.agent.toString, pursuerScore, captureEvents)
  }

  /**
    * Return next Competition result.
    */
  def update(evaderState: PEState, pursuerState: PEState, captureEvent: Boolean = false, survivalEvent: Boolean = false) = {

    // State lists should be same size
    assert(evaderStates.size == pursuerStates.size)

    // Calculate score updates if we have state history
    val evaderStateWithScore = if (evaderStates.nonEmpty) {
      evaderState.agent.getScore(evaderState, evaderStates.head, pursuerState, pursuerStates.head, captureEvent, survivalEvent)
    }
    else {
      evaderState
    }

    val pursuerStateWithScore = if (pursuerStates.nonEmpty) {
      pursuerState.agent.getScore(pursuerState, pursuerStates.head, evaderState, evaderStates.head, captureEvent, survivalEvent)
    }
    else {
      pursuerState
    }

    debug(s"EvaderScore ${evaderStateWithScore.score}")
    debug(s"PursuerScore ${pursuerStateWithScore.score}")

    PECompetitionResult(
      evaderStateWithScore :: evaderStates,
      pursuerStateWithScore :: pursuerStates,
      evaderStateWithScore.score,
      pursuerStateWithScore.score,
      if (survivalEvent) survivalEvents + 1 else survivalEvents,
      if (captureEvent) captureEvents + 1 else captureEvents)
  }

  /**
    * Publish results
    */
  def publish(withHistory: Boolean) = {

    val evaderHistory = evaderStates.reverse
    val pursuerHistory = pursuerStates.reverse

    PECompetitionResult(
      if (withHistory) evaderHistory else List(evaderHistory.head),
      if (withHistory) pursuerHistory else List(pursuerHistory.head),
      evaderScore,
      pursuerScore,
      survivalEvents,
      captureEvents)
  }

  /**
    * Expand Compeition result by expansionRatio.
    *
    * Used for animation purposes.
    */
  def expand(expansionRatio: Int) = {
    PECompetitionResult(PEState.expandStates(evaderStates, expansionRatio),
      PEState.expandStates(pursuerStates, expansionRatio),
      evaderScore,
      pursuerScore,
      survivalEvents,
      captureEvents)
  }
}