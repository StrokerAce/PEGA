package com.matthewtyler.pe.test.strategy.pestrategy

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.math.Vector
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.pestrategy.{PEStrategyHelper, RadialBucketContainer}
import com.matthewtyler.pe.test.constraints.TestConstraints
import org.scalatest.{Matchers, Suite}

/**
 * RadialBucket test suite.
 */
class RadialBucketTestSuite extends Suite with Matchers {

  /**
   * Test new random RadialBucketContainer functionality.
   */
  def testRandomRadialBucketContainer = {
    
    // Get random RadialBucketContainer
    val randomRadialBucketContainer = RadialBucketContainer.newRandom(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
    
    // Get the RadialBucket List from the container
    val radialBuckets = randomRadialBucketContainer.radialBuckets
    
    // Validate the number of buckets is correct
    radialBuckets.size should equal(TestConstraints.evaderConstraints.radialBuckets)
    
    // Validate RadialBucketContainer structure.
    PEStrategyHelper.validateRadialBucketContainer(randomRadialBucketContainer) should be(true)
    
    // Validate each bucket contains a vector with magnitude == radiusMin
    // Validate each bucket does not contain a vector with magnitude == radiusmax
    for(bucket <- radialBuckets) {
    
      // Validate bucket contains lower boundary
      val lowerBoundVector = Vector(bucket.radiusMin,0.0)
      val origin = Vector(0.0,0.0)
      
      val evader = Agent.newRandomEvader(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
      
      val lowerBoundState = PEState(lowerBoundVector,origin,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
      
      bucket.applies(lowerBoundState) should be(true)
      
      // Validate bucket does not contain upper boundary
      val upperBoundVector = Vector(bucket.radiusMax,0.0)
      
      val upperBoundState = PEState(upperBoundVector,origin,evader,evader.constraints.startingEnergy,evader.constraints.winBonus)
      
      bucket.applies(upperBoundState) should be(false)
    }   
  }
  
  /**
   * Test RadialBucket mate.
   */
  def testMate = {
    
    for(i <- 1 to 100) {

      val parent1 = RadialBucketContainer.newRandom(TestConstraints.evaderConstraints,TestConstraints.tournamentConstraints)
      val parent2 = RadialBucketContainer.newRandom(TestConstraints.pursuerConstraints,TestConstraints.tournamentConstraints)

      val offspring = parent1 mate parent2

      // Validate parents
      PEStrategyHelper.validateRadialBucketContainer(parent1) should be(true)
      PEStrategyHelper.validateRadialBucketContainer(parent2) should be(true)
      
      // Validate offspring is same size as parents
      parent1.radialBuckets.size should equal(offspring.radialBuckets.size)

      // Validate offspring
      PEStrategyHelper.validateRadialBucketContainer(offspring) should be(true)
      
      var cumulativeRadius = 0L

      // Validate offspring bucket boundaries
      for (bucket <- offspring.radialBuckets) {

        bucket.radiusMin should equal(cumulativeRadius)
        bucket.radiusMax should equal(cumulativeRadius + bucket.size)

        cumulativeRadius += bucket.size
      }
    }
  }
}

/**
 * Run the test suite
 */
object RadialBucketTestSuite extends App {
  (new RadialBucketTestSuite).execute()
}