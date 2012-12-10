package com.matthewtyler.pe.test.strategy

import org.scalatest.Suite

import com.matthewtyler.pe.test.strategy.pestrategy._
import com.matthewtyler.pe.test.strategy.tactic.PETacticTestSuite

class StrategyTestSuite extends Suite {
  
  (new AngularBucketTestSuite).execute()
  (new PEStrategyTestSuite).execute()
  (new RadialBucketTestSuite).execute()
  
  (new PETacticTestSuite).execute()
}

/**
 * Run the test suite.
 */
object StrategyTestSuite extends App {
  (new StrategyTestSuite).execute()
}