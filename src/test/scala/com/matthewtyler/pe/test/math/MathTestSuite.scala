package com.matthewtyler.pe.test.math

import org.scalatest.Suite

/**
 * Math test suite
 */
class MathTestSuite extends Suite {

  (new MathHelperTestSuite).execute()
  (new VectorTestSuite).execute()
}

/**
 * Run the test suite
 */
object MathTestSuite extends App {
  (new MathTestSuite).execute()
}