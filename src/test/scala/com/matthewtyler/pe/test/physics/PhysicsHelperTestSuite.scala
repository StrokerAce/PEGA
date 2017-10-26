package com.matthewtyler.pe.test.physics

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.physics.PhysicsHelper
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * PhysicsHelper test suite.
 */
class PhysicsHelperTestSuite extends Suite with Logging with Matchers {
  
  /**
   * Test accelerationForEnergy method.
   */
  def testAccelerationForEnergy = {
    
    // E = 0.5 * m * a^2
    val mass = 10.0
    
    // Positive energy.
    PhysicsHelper.accelerationForEnergy(mass,20.0) should equal (2.0)
    
    // Zero energy.
    PhysicsHelper.accelerationForEnergy(mass,0.0) should equal(0.0)
  }
  
  /**
   * Test actualAcceleration method.
   */
  def testActualAcceleration = {
    
    // F = ma
    // dE = 0.5 * m * a^2
    
    val mass = 10.0
    val energy = 1000.0
    
    // Force + Energy sufficient for required acceleration
    PhysicsHelper.actualAcceleration(mass,energy,10.0,1.0) should equal(1.0)
    
    // Force sufficient for half required acceleration
    PhysicsHelper.actualAcceleration(mass,energy,5.0,1.0) should equal(0.5)
    
    // Energy sufficient for half required acceleration
    PhysicsHelper.actualAcceleration(mass,20.0,40.0,4.0) should equal(2.0)
    
    // Zero force
    PhysicsHelper.actualAcceleration(mass,energy,0.0,1.0) should equal(0.0)
    
    // Zero energy
    PhysicsHelper.actualAcceleration(mass,0.0,10.0,1.0) should equal(0.0)
  }
  
  /**
   * Test energyToAccelerate method.
   */
  def testEnergyToAccelerate = {
    
    val mass = 100
    
    // Positive acceleration case
    // 0.5 * m * v^2
    PhysicsHelper.energyToAccelerate(mass,10.0) should equal(5000.0)
    
    // Negative acceleration case
    // 0.5 * m * v^2
    PhysicsHelper.energyToAccelerate(mass,-10.0) should equal(5000.0)
    
    // Zero acceleration case
    PhysicsHelper.energyToAccelerate(mass,0.0) should equal (0.0)
  }
  
  /**
   * Test maxRotationForSpeed
   */
  def testMaxRotationForSpeed = {
    
    val constraints = TestConstraints.evaderConstraints
    
    // Max speed case.
    PhysicsHelper.maxRotationForSpeed(constraints.maxSpeed,constraints) should equal(constraints.maxAngularVelocityAtMaxSpeed * constraints.maxAngularVelocity)
    
    // Half speed case
    PhysicsHelper.maxRotationForSpeed(constraints.maxSpeed * 0.5,constraints) should equal( ((0.5 * (1 - constraints.maxAngularVelocityAtMaxSpeed)) + constraints.maxAngularVelocityAtMaxSpeed) * constraints.maxAngularVelocity )
    
    // Zero speed case
    PhysicsHelper.maxRotationForSpeed(0.0,constraints) should equal(0.0)    
  }
}

/**
 * Run the test suite.
 */
object PhysicsHelperTestSuite extends App {
  
  (new PhysicsHelperTestSuite).execute()
}