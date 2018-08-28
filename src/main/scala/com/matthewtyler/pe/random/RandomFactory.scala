package com.matthewtyler.pe.random

import scala.util.Random

/**
  * Random number source.
  */
object RandomFactory {

  import scala.math._

  // Random seed
  // TODO configure from gui?.
  private val randomSeed = 12345678

  // Random generator
  // Public so users can access methods already defined.
  val randomGenerator = new Random(randomSeed)

  /**
    * Next double in range 0.0..range inclusive.
    */
  def nextDoubleInRange(range: Double) = randomGenerator.nextDouble * range

  /**
    * Next Int in range 1..range inclusive
    */
  def nextIntInRange(range: Int) = (abs(randomGenerator.nextInt) % range) + 1

  /**
    * Returns +1 or -1
    */
  def nextSignedUnit = if (randomGenerator.nextBoolean) 1 else -1

  /**
    * Randomly selects value from choices.
    */
  def randomPick[T](choices: T*) = choices(randomGenerator.nextInt(choices.size))
}