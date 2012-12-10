package com.matthewtyler.pe.test.math

import org.scalatest.Suite
import org.scalatest.matchers.MustMatchers

import com.matthewtyler.pe.math.MathHelper

/**
 * Unit tests for MathHelper functions
 */
class MathHelperTestSuite extends Suite with MustMatchers {

  import scala.math._
  
  /**
   * Test angleBetween method.
   */
  def testAngleBetween = {
    
    // Positive direction.
    MathHelper.angleBetween(Pi * 0.5,Pi,1.0) must be(Pi * 0.5 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Positive direction.
    MathHelper.angleBetween(Pi,Pi * 0.5,1.0) must be(Pi * 1.5 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Negative direction.
    MathHelper.angleBetween(Pi * 0.5, Pi,-1.0) must be(Pi * 1.5 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Positive direction.
    MathHelper.angleBetween(Pi * 0.5,Pi,1.0) must be(Pi * 0.5 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Positive Direction
    MathHelper.angleBetween(Pi * 0.25,Pi * 1.75,1.0) must be(Pi * 1.5 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Positive Direction
    MathHelper.angleBetween(Pi * 0.25,Pi * 1.75,-1.0) must be(Pi * 0.5 plusOrMinus MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test doubleEquals method
   */
  def testDoubleEquals = {
    
    // Difference slightly < EPS_DOUBLE
    MathHelper.doubleEquals(1.0, 1.0 - (MathHelper.EPS_DOUBLE / 10)) must be(true)
    
    // Difference slightly > EPS_DOUBLE
    MathHelper.doubleEquals(1.0, 1.0 - (MathHelper.EPS_DOUBLE * 1.01)) must be(false)
  }
  
  /**
   * test getR method.
   */
  def testGetR = {
  
    // Zero magnitude vector should have angle of zero.
    MathHelper.getR(0.0,0.0) must be(0.0 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Vector in +i direction should have angle of zero
    MathHelper.getR(1.0,0.0) must be(0.0 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Vector in +j direction must have angle of Pi / 2 
    MathHelper.getR(0.0,1.0) must be(Pi/2 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Vector in -i direction should have angle of Pi
    MathHelper.getR(-1.0,0.0) must be(Pi plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Vector in -j direction must have angle of 3Pi / 2 
    MathHelper.getR(0.0,-1.0) must be(3 * (Pi/2) plusOrMinus MathHelper.EPS_DOUBLE)    
  }
  
  /**
   * Test normaliseAngle method.
   */
  def testNormaliseAngle = {
    
    // Zero must normalise to zero
    MathHelper.normaliseAngle(0.0) must be(0.0 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // Pi/2 must normalise to Pi/2
    MathHelper.normaliseAngle(Pi/2) must be(Pi/2 plusOrMinus MathHelper.EPS_DOUBLE)
    
    // -Pi must normalise to Pi
    MathHelper.normaliseAngle(-Pi) must be(Pi plusOrMinus MathHelper.EPS_DOUBLE)
    
    // -Pi/2 must normalise to 3Pi/2
    MathHelper.normaliseAngle(-(Pi / 2)) must be((3 * Pi)/2 plusOrMinus MathHelper.EPS_DOUBLE) 
    
    // 100 Pi must normalise to 0.0
    MathHelper.normaliseAngle(Pi * 100) must be(0.0 plusOrMinus MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test truncateDouble method.
   */
  def testTruncateDouble = {
    
    // < EPS_DOUBLE to be truncated
    MathHelper.truncateDouble(MathHelper.EPS_DOUBLE / 10) must equal(0.0)
    
    // >= EPS_DOUBLE not to be truncated
    MathHelper.truncateDouble(MathHelper.EPS_DOUBLE) must not equal(0.0)  
  }
}

object MathHelperTestSuite extends App {
  (new MathHelperTestSuite).execute()
}