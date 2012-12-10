package com.matthewtyler.pe.constraints

import com.matthewtyler.pe.random.RandomFactory

/**
 * Tournament Constraints
 */
case class TournamentConstraints(generations       : Int,
                                 population        : Int,
                                 competitionHeats  : Int,
                                 startDistance     : Double,
                                 captureDistance   : Double,
                                 mutationRate      : Double,
                                 agentsToCull      : Int,
                                 competitionActors : Int,
                                 randomSeed        : Long) {
  
  /**
   * Returns true if mutation should occur.
   */
  def mutate = RandomFactory.randomGenerator.nextDouble < mutationRate
  
}