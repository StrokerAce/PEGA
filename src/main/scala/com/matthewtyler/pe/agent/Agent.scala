package com.matthewtyler.pe.agent

import com.matthewtyler.pe.constraints.{AgentConstraints, TournamentConstraints}
import com.matthewtyler.pe.helper.IdSource
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.pestrategy.PEStrategy

/**
  * Class defines GA agent.
  */
class Agent(agentType: String,
            private val id: Long,
            val constraints: AgentConstraints,
            val strategy: PEStrategy) extends Instruction with Logging with Mateable[Agent] with Ordered[Agent] {

  // String representation of this Agent
  lazy val agentString = s"$agentType $id"

  /**
    * Implement Instruction trait
    */
  def applyInstruction(myState: PEState, opponentState: PEState) =
    strategy.applyInstruction(myState, PEState.toFrameOfReference(myState, opponentState))

  /**
    * Implement Ordering trait
    */
  implicit def compare(other: Agent) = id.compare(other.id)

  /**
    * Override equals method.
    */
  override def equals(other: Any) = other match {
    case that: Agent => agentString equals that.agentString
    case _ => false
  }

  /**
    * Get Agent score for this update.
    */
  def getScore(myCurrentState: PEState,
               myPreviousState: PEState,
               opponentCurrentState: PEState,
               opponentPreviousState: PEState,
               captureEvent: Boolean = false,
               survivalEvent: Boolean = false) = {

    constraints.updateStateWithScore(
      myCurrentState,
      myPreviousState,
      opponentCurrentState,
      opponentPreviousState,
      agentType match {
        case "Evader" => survivalEvent
        case "Pursuer" => captureEvent
      })
  }

  /**
    * Implement Mateable trait.
    *
    * Mate this Agent with p.
    */
  def mate(p: Agent) = new Agent(agentType, Agent.nextId, constraints, strategy mate p.strategy)

  /**
    * Override toString method.
    */
  override def toString = agentString
}


/**
  * Factory methods and helpers.
  */
object Agent extends IdSource with Logging {

  /**
    * Get new random Evader Agent
    */
  def newRandomEvader(agentConstraints: AgentConstraints, tournamentConstraints: TournamentConstraints) =
    new Agent("Evader", nextId, agentConstraints, PEStrategy.newRandom(agentConstraints, tournamentConstraints))

  /**
    * Get new random Pursuer Agent
    */
  def newRandomPursuer(agentConstraints: AgentConstraints, tournamentConstraints: TournamentConstraints) =
    new Agent("Pursuer", nextId, agentConstraints, PEStrategy.newRandom(agentConstraints, tournamentConstraints))
}