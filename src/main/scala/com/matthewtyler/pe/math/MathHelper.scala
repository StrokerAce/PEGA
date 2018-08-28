package com.matthewtyler.pe.math

/**
 * Various Math/Vector helper functions.
 */
object MathHelper {
  
  import scala.math._
  
  // Double epsilon
  val EPS_DOUBLE = 0.00000000001
  
  // Two Pi
  val TWO_PI = Pi * 2.0
  
  // Half Pi
  val HALF_PI = Pi / 2
  
  /**
   * Returns the angle between two angles in the specified direction.
   * Assumes angles are normalised to range 0..2PI
   */
  def angleBetween(angle1 : Double,angle2 : Double,direction : Double) = {
    
    assert(doubleEquals(abs(direction),1.0))
    
    if(doubleEquals(direction,-1.0)) {
      if(angle1 > angle2) angle1 - angle2 else TWO_PI - (angle2 - angle1)
    }
    else {
      if(angle1 > angle2) TWO_PI - (angle1 - angle2) else angle2 - angle1
    }
  }
  
  /**
   * Calculates the minumum angle between two angles
   */
  def closestAngleBetween(angle1 : Double,angle2 : Double) = {
    
    val n1 = normaliseAngle(angle1)
    val n2 = normaliseAngle(angle2)
    
    max(n1,n2) - min(n1,n2)
  }
  
  /**
   * Returns true if the absolute difference between two Doubles is less than EPS_DOUBLE.
   */
  def doubleEquals(a : Double,b : Double) = abs(a - b) < EPS_DOUBLE
  
  /**
   * Returns true if two values are equal or left < right
   */
  def <= (left : Double,right : Double) = doubleEquals(right - left,0.0) || left <= right
  
  /**
   * Returns true if two values are equal or left > right
   */
  def >= (left : Double,right : Double) = doubleEquals(left - right,0.0) || left >= right  
  
  /**
   * Get radial component of polar translated from i,j in range 0..2 Pi
   */
  def getR(i : Double,j : Double) = {
    
    val theta = atan2(j,i)
    
    if(theta < 0) TWO_PI + theta else theta
  }
  
  /**
   * Convert angle to range 0..2Pi
   */
  def normaliseAngle(theta : Double) = {
        
    val converted = atan2(sin(theta),cos(theta))
    
    if(converted < 0) TWO_PI + converted else converted
  }
  
  /**
   * Truncate double to zero if less then EPS_Double
   */
  def truncateDouble(d : Double) = if(abs(d) < EPS_DOUBLE) 0.0 else d
}