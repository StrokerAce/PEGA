package com.matthewtyler.pe.test.math

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.math.Vector

/**
 * Unit tests for Vector class and helpers.
 */
class VectorTestSuite extends Suite with Matchers {
  
  import scala.math._
  
  /**
   * Test Vector +
   */
  def testAddition = {
    
    val vector1 = new Vector(2.0,0.0)
    val vector2 = new Vector(0.0,2.0)
    
    val sum = vector1 + vector2
    
    sum.i should equal(2.0)
    sum.j should equal(2.0)
    sum.magnitude should equal(sqrt(8.0))
    sum.theta should equal(Pi / 4.0)
  }
  
  /**
   * Test Vector addMagnitude method. 
   */
  def testAddMagnitude() {
    
    val vector1 = Vector(3.0,4.0)
    
    val increasedVector1 = vector1 addMagnitude 1.0
    
    increasedVector1.magnitude should equal(6.0)
    
    val oppositeVector1 = vector1 addMagnitude -10.0
    
    oppositeVector1.magnitude should equal(5.0)
  }
  
  /**
   * Test Vector -
   */
  def testSubtraction = {
    
    val vector1 = new Vector(0.0,2.0)
    val vector2 = new Vector(2.0,0.0)
    
    val diff = vector1 - vector2
    
    diff.i should equal(-2.0)
    diff.j should equal(2.0)
    diff.magnitude should be(sqrt(8.0) +- MathHelper.EPS_DOUBLE)
    diff.theta should be(Pi * (3.0/4.0) +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test Vector angleBetween
   */
  def testAngleBetween = {
   
    val vector1 = new Vector(1.0,1.0)
    val vector2 = new Vector(-1.0,1.0)
    
    vector1 angleBetween vector2 should equal(MathHelper.HALF_PI)
    
    val vector3 = new Vector(0.0,-1.0)
    vector1 angleBetween vector3 should be(Pi * (3.0/4.0) +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test Vector scalarProduct
   */
  def testScalarProduct = {
    
    val vector1 = new Vector(1.0,0.0)
    val vector2 = new Vector(11.0,0.0)
    
    vector1 scalarProduct vector2 should equal(11.0)
    
    val vector3 = new Vector(0.0,99.5)
    
    vector1 scalarProduct vector3 should equal(0.0)
  }
  
  /**
   * Test Vector rotate
   */
  def testRotate = {
    
    val vector1 = new Vector(1.0,0.0)
    
    val rotated1 = vector1 rotate (Pi * (3.0/4.0))
    
    rotated1.i should be(-sqrt(0.5) +- MathHelper.EPS_DOUBLE)
    rotated1.j should be(sqrt(0.5) +- MathHelper.EPS_DOUBLE)
    rotated1.magnitude should be(1.0 +- MathHelper.EPS_DOUBLE)
    rotated1.theta should be(Pi * (3.0/4.0) +- MathHelper.EPS_DOUBLE)
    
    val rotated2 = vector1 rotate (-Pi / 4)
    
    MathHelper.getR(rotated2.i, rotated2.j)
    
    rotated2.i should be(sqrt(0.5) +- MathHelper.EPS_DOUBLE)
    rotated2.j should be(-sqrt(0.5) +- MathHelper.EPS_DOUBLE)
    rotated2.magnitude should be(1.0 +- MathHelper.EPS_DOUBLE)
    rotated2.theta should be(MathHelper.TWO_PI - (Pi / 4) +- MathHelper.EPS_DOUBLE)
  }
  
  /**
   * Test Vector scale
   */
  def testScale = {
    
    val vector1 = new Vector(1.0,2.0)
    
    val scaled = vector1 scale Pi
    
    scaled.i should equal(vector1.i * Pi)
    scaled.j should equal(vector1.j * Pi)
    scaled.magnitude should equal(vector1.magnitude * Pi)
    scaled.theta should equal(vector1.theta)
  }
  
  /** 
   * Test Vector unitVector
   * */
  def testUnitVector = {
    
    val vector1 = new Vector(Pi,Pi)
    
    val unitVector = vector1.unitVector
    
    unitVector.magnitude should equal(1.0)
    unitVector.theta should equal(Pi / 4.0)
  }
}

/*
 * Run the test suite.
 */
object VectorTestSuite extends App {
  (new VectorTestSuite).execute()
}