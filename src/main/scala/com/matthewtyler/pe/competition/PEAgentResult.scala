package com.matthewtyler.pe.competition

/**
 * PEAgentResultClass
 * 
 * Encapsulates the result of a competition.
 */
case class PEAgentResult(agent : String,score : Double,bonusEvents : Int,competitions : Int = 1) {

  // Average score
  val averageScore = score / competitions
  
  private val agentResultString = "PEAgentResult: Agent %s, Score %s, Bonus Events %s, Competitions %s, Average %s".format(agent,score,bonusEvents,competitions,averageScore)
  
  /**
   * + method
   */
  def +(other : PEAgentResult) = {
    
    if(agent != other.agent)
      throw new IllegalArgumentException("Attempted to add PEAgentResults different agents %s, %s.".format(agent,other.agent))
    
    PEAgentResult(agent,score + other.score,bonusEvents + other.bonusEvents,competitions + other.competitions)   
  }
  
  /**
   * < method.
   */
  def <(other : PEAgentResult) = score < other.score
  
  /**
   * Override equals method
   */
  override def equals(other : Any) = other match {
    case that : PEAgentResult => agent equals that.agent
    case _ => false
  }
  
  /**
   * Override toString method.
   */
  override def toString = agentResultString
}