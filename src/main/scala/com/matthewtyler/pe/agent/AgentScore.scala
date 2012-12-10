package com.matthewtyler.pe.agent

/**
 * Container for agent score pairs
 */
case class AgentScore(agent : Agent,
                      score : Double) {
  /**
   * Override toString method.
   */
  override def toString() = agent.toString
  
}
                      
/**
 * Sort functions for AgentScores
 */
object AgentScore {
  
  /**
   * Sort by decreasing agent score.
   */
  def compare(left : AgentScore,right : AgentScore) = left.score > right.score
  
}