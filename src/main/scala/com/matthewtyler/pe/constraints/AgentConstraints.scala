package com.matthewtyler.pe.constraints

import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.state.pestate.PEState

/**
  * Agent Constraints.
  */
case class AgentConstraints(agentType: String,
                            angularBuckets: Int,
                            radialBuckets: Int,
                            maxRadialBucketSize: Int,
                            maxForce: Double,
                            maxSpeed: Double,
                            maxAngularVelocity: Double,
                            maxAngularVelocityAtMaxSpeed: Double,
                            mass: Double,
                            startingEnergy: Double,
                            winBonus: Double,
                            pointsPerPositionImprovement: Double,
                            bonusPenalty: Double,
                            addRemainingEnergyToWinningScore: Boolean,
                            improvementPredicate: Double => Boolean) extends Logging {

  import scala.math._

  val maxAcceleration = maxForce / mass

  /*
   * Calculate agent score.
   */
  def updateStateWithScore(myCurrentState: PEState,
                           myPreviousState: PEState,
                           opponentCurrentState: PEState,
                           opponentPreviousState: PEState,
                           winEvent: Boolean = false): PEState = {

    val currentDistance = (myCurrentState.position - opponentCurrentState.position).magnitude
    val previousDistance = (myPreviousState.position - opponentPreviousState.position).magnitude
    val deltaDistance = currentDistance - previousDistance

    val winScoreComponent = if (winEvent) {
      debug("Win event.")
      val energyBonus = if (addRemainingEnergyToWinningScore) myCurrentState.energy else 0.0
      myPreviousState.remainingBonus + energyBonus
    }
    else
      0.0

    val improvementScoreComponent = if (improvementPredicate(deltaDistance) && myCurrentState.isActive) pointsPerPositionImprovement else 0.0

    // Enrich current state with score information.
    PEState(myCurrentState.position,
      myCurrentState.velocity,
      myCurrentState.agent,
      myCurrentState.energy,
      max(0.0, myPreviousState.remainingBonus - bonusPenalty),
      myPreviousState.score + winScoreComponent + improvementScoreComponent,
      winEvent)
  }
}