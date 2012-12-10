package com.matthewtyler.pe.test.strategy.pestrategy

import org.scalatest.Suite
import org.scalatest.matchers.MustMatchers

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.math.Vector
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.pestrategy.AngularBucket
import com.matthewtyler.pe.strategy.pestrategy.AngularBucketContainer
import com.matthewtyler.pe.strategy.pestrategy.PEStrategyHelper
import com.matthewtyler.pe.test.constraints.TestConstraints


/**
 * AngularBucket test suite.
 */
class AngularBucketTestSuite extends Suite with MustMatchers {
  
  import scala.math._
  
  /**
   * Test new random AngularBucketContainer functionality.
   */
  def testRandomAngularBucketContainer = {
    
    // Get random AngularBucketContainer
    val randomAngularBucketContainer = AngularBucketContainer.newRandom(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    
    // Get the AngularBuckets in the container
    val angularBuckets = randomAngularBucketContainer.angularBuckets
    
    // Validate the number of buckets is correct
    angularBuckets.size must equal(TestConstraints.evaderConstraints.angularBuckets)
    
    // Validate AngularBucketContainer structure.
    PEStrategyHelper.validateAngularBucketContainer(randomAngularBucketContainer) must be(true)
        
    // Validate first bucket contains two pi.
    val twoPiVector = Vector(cos(MathHelper.TWO_PI),sin(MathHelper.TWO_PI))
    val origin = Vector(0.0, 0.0)
    
    val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
          
    val twoPiState = PEState(twoPiVector,origin,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
      
    angularBuckets.head.applies(twoPiState) must be(true)
  }
  
  /**
   * Test AngularBucketContainer mate.
   */
  def testMate = {
    
    for(i <- 1 to 100) {
      val parent1 = AngularBucketContainer.newRandom(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
      val parent2 = AngularBucketContainer.newRandom(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
      
      // Validate parents
      PEStrategyHelper.validateAngularBucketContainer(parent1) must be(true)
      PEStrategyHelper.validateAngularBucketContainer(parent2) must be(true)

      val offspring = parent1 mate parent2

      // Validate offspring is same size as parents
      parent1.angularBuckets.size must equal(offspring.angularBuckets.size)

      // Validate offspring AngularBucketContainer structure. 
      PEStrategyHelper.validateAngularBucketContainer(offspring) must be (true)
    }
  }
}

/**
 * Run the test suite.
 */
object AngularBucketTestSuite extends App {
  (new AngularBucketTestSuite).execute()
}