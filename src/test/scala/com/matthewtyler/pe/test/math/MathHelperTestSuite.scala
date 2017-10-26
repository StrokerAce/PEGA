package com.matthewtyler.pe.test.math

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.math.MathHelper

/**
 * Unit tests for MathHelper functions
 */
class MathHelperTestSuite extends Suite with Matchers {

  import scala.math._
  
  /**
   * Test angleBetween method.
   */
  def testAngleBetween = {
    
    // Positive direction.
    MathHelper.angleBetween(Pi * 0.5,Pi,1.0) should be(Pi * 0.5 +- MathHelper.EPS_DOUBLE)
    
    // Positive direction.
    MathHelper.angleBetween(Pi,Pi * 0.5,1.0) should be(Pi * 1.5 +- MathHelper.EPS_DOUBLE)
    
    // Negative direction.
    MathHelper.angleBetween(Pi * 0.5, Pi,-1.0) should be(Pi * 1.5 +- MathHelper.EPS_DOUBLE)
    
    // Positive direction.
    MathHelper.angleBetween(Pi * 0.5,Pi,1.0) should be(Pi * 0.5 +- MathHelper.EPS_DOUBLE)
    
    // Positive Direction
    MathHelper.angleBetween(Pi * 0.25,Pi * 1.75,1.0) should be(Pi * 1.5 +- MathHelper.EPS_DOUBLE)
    
    // Positive Direction
    MathHelper.angleBetween(Pi * 0.25,Pi * 1.75,-1.0) should be(Pi * 0.5 +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test doubleEquals method
   */
  def testDoubleEquals = {
    
    // Difference slightly < EPS_DOUBLE
    MathHelper.doubleEquals(1.0, 1.0 - (MathHelper.EPS_DOUBLE / 10)) should be(true)
    
    // Difference slightly > EPS_DOUBLE
    MathHelper.doubleEquals(1.0, 1.0 - (MathHelper.EPS_DOUBLE * 1.01)) should be(false)
  }
  
  /**
   * test getR method.
   */
  def testGetR = {
  
    // Zero magnitude vector should have angle of zero.
    MathHelper.getR(0.0,0.0) should be(0.0 +- MathHelper.EPS_DOUBLE)
    
    // Vector in +i direction should have angle of zero
    MathHelper.getR(1.0,0.0) should be(0.0 +- MathHelper.EPS_DOUBLE)
    
    // Vector in +j direction should have angle of Pi / 2 
    MathHelper.getR(0.0,1.0) should be(Pi/2 +- MathHelper.EPS_DOUBLE)
    
    // Vector in -i direction should have angle of Pi
    MathHelper.getR(-1.0,0.0) should be(Pi +- MathHelper.EPS_DOUBLE)
    
    // Vector in -j direction should have angle of 3Pi / 2 
    MathHelper.getR(0.0,-1.0) should be(3 * (Pi/2) +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test normaliseAngle method.
   */
  def testNormaliseAngle = {
    
    // Zero should normalise to zero
    MathHelper.normaliseAngle(0.0) should be(0.0 +- MathHelper.EPS_DOUBLE)
    
    // Pi/2 should normalise to Pi/2
    MathHelper.normaliseAngle(Pi/2) should be(Pi/2 +- MathHelper.EPS_DOUBLE)
    
    // -Pi should normalise to Pi
    MathHelper.normaliseAngle(-Pi) should be(Pi +- MathHelper.EPS_DOUBLE)
    
    // -Pi/2 should normalise to 3Pi/2
    MathHelper.normaliseAngle(-(Pi / 2)) should be((3 * Pi)/2 +- MathHelper.EPS_DOUBLE)
    
    // 100 Pi should normalise to 0.0
    MathHelper.normaliseAngle(Pi * 100) should be(0.0 +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test truncateDouble method.
   */
  def testTruncateDouble = {
    
    // < EPS_DOUBLE to be truncated
    MathHelper.truncateDouble(MathHelper.EPS_DOUBLE / 10) should equal(0.0)
    
    // >= EPS_DOUBLE not to be truncated
    MathHelper.truncateDouble(MathHelper.EPS_DOUBLE) should not equal(0.0)  
  }
}

object MathHelperTestSuite extends App {
  (new MathHelperTestSuite).execute()
}