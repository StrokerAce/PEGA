package com.matthewtyler.pe.math

import com.matthewtyler.pe.random.RandomFactory

import scala.math._

/**
  * Vector class.
  * Represents Cartesian/Polar vector.
  */
case class Vector(iVal: Double, jVal: Double) {

  // Cartesian components
  val i = MathHelper.truncateDouble(iVal)
  val j = MathHelper.truncateDouble(jVal)

  // Polar components
  val magnitude = sqrt((i * i) + (j * j))
  val theta = MathHelper.getR(i, j)

  // String representation
  private lazy val vectorString = s"Vector\n------\ni         : $i\nj         : $j\nmagnitude : $magnitude\ntheta     : $theta\n------\n"

  /**
    * Returns new Vector which is the vector sum of this Vector and v.
    */
  def +(v: Vector) = Vector(i + v.i, j + v.j)

  /**
    * Returns new Vector which is the vector difference of this Vector and v.
    */
  def -(v: Vector) = Vector(i - v.i, j - v.j)


  /**
    * Return new vector which is the sum of this vector + unit vector
    * in the direction (or opposite direction) of this vector scaled by input magnitude.
    */
  def addMagnitude(magnitude: Double) = this + unitVector.scale(magnitude)

  /**
    * Returns the angle between this Vector and v.
    *
    * From:
    *
    * A.B = |A||B|cos(Theta)
    *
    */
  def angleBetween(v: Vector) = acos((this scalarProduct v) / (magnitude * v.magnitude))

  /**
    * Override equals.
    * Implicit case class version not floating point savvy.
    */
  override def equals(other: Any) = other match {
    case that: Vector => MathHelper.doubleEquals(i, that.i) && MathHelper.doubleEquals(j, that.j)
    case _ => false
  }

  /**
    * Rotate this Vector by theta around origin.
    *
    * From:
    *
    * x' = x Cos(theta) - y Sin(theta)
    * y' = x Sin(theta) + y Cos(theta)
    *
    */
  def rotate(rotationAngle: Double) = {

    val normalisedTheta = MathHelper.normaliseAngle(theta + rotationAngle)

    Vector(cos(normalisedTheta) * magnitude, sin(normalisedTheta) * magnitude)
  }

  /**
    * Returns the scalar product of this Vector and v.
    */
  def scalarProduct(v: Vector) = (i * v.i) + (j * v.j)

  /**
    * Returns new Vector which is this Vector scaled by scaleFactor.
    */
  def scale(scaleFactor: Double) = Vector(i * scaleFactor, j * scaleFactor)

  /**
    * Override toString method.
    */
  override def toString = vectorString

  /**
    * Returns new unit Vector in the direction of this Vector
    */
  lazy val unitVector = {
    // If vector has zero magnitude return unit vector in i direction.
    if (MathHelper.doubleEquals(i, 0.0) && MathHelper.doubleEquals(j, 0.0)) new Vector(1.0, 0.0) else new Vector(i / magnitude, j / magnitude)
  }

  /**
    * Returns new Vector which is the vector product of this vector and v.
    */
  def vectorProduct(v: Vector) = Vector(0, 0)
}

/**
  * Vector helpers
  */
object Vector {

  /**
    * Validate Vector components.
    *
    * Used in unit tests.
    */
  def isValid(vector: Vector) = {
    vector.magnitude != Double.NaN &&
      vector.magnitude >= 0.0 &&
      vector.theta != Double.NaN &&
      vector.theta >= 0 &&
      vector.theta <= MathHelper.TWO_PI &&
      vector.i != Double.NaN &&
      vector.j != Double.NaN
  }

  /**
    * Random vector with specified magnitude.
    * I.e random orientation.
    *
    * Magnitude must be >= 0.0.
    */
  def randomVectorConstMagnitude(magnitude: Double) = {

    assert(magnitude >= 0.0)

    val theta = RandomFactory.nextDoubleInRange(MathHelper.TWO_PI)

    Vector(magnitude * cos(theta), magnitude * sin(theta))
  }

  /**
    * Random vector with random magnitude in range 0..maxMagnitude.
    */
  def randomVectorRandomMagnitude(maxMagnitude: Double) = randomVectorConstMagnitude(RandomFactory.nextDoubleInRange(maxMagnitude))
}
