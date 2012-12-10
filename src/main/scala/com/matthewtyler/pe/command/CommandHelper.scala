package com.matthewtyler.pe.command

/**
 * CommandHelper object.
 */
object CommandHelper {
  
  // Direction string representation.
  private val POSITIVE = "POSITIVE"
  private val NEGATIVE = "NEGATIVE"

  // Directions
  val directionNegative = -1
  val directionPositive = 1
  
  /**
   * Returns POSITIVE or NEGATIVE depending on sign of direction
   */
  def directionToString(direction : Int) = if(direction < 0) POSITIVE else NEGATIVE 
}