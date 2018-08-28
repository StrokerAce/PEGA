package com.matthewtyler.pe.competition

/**
  * PEAgentResultClass
  *
  * Encapsulates the result of a competition.
  */
case class PEAgentResult(agent: String, score: Double, bonusEvents: Int, competitions: Int = 1) {

  // Average score
  val averageScore = score / competitions

  private lazy val agentResultString = s"PEAgentResult: Agent $agent, Score $score, Bonus Events $bonusEvents, Competitions $competitions, Average $averageScore"

  /**
    * + method
    */
  def +(other: PEAgentResult) = {

    if (agent != other.agent)
      throw new IllegalArgumentException(s"Attempted to add PEAgentResults different agents $agent, ${other.agent}.")

    PEAgentResult(agent, score + other.score, bonusEvents + other.bonusEvents, competitions + other.competitions)
  }

  /**
    * < method.
    */
  def <(other: PEAgentResult) = score < other.score

  /**
    * Override equals method
    */
  override def equals(other: Any) = other match {
    case that: PEAgentResult => agent equals that.agent
    case _ => false
  }

  /**
    * Override toString method.
    */
  override def toString = agentResultString
}