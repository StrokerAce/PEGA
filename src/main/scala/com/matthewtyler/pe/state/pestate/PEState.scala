package com.matthewtyler.pe.state.pestate

import scala.annotation.tailrec

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.math.{MathHelper, Vector}
import com.matthewtyler.pe.physics.PhysicsHelper

/**
 * PEState class
 * 
 * Encapsulates the state of an Agent.
 */
case class PEState(val position : Vector, 
                   val velocity : Vector,
                   val agent : Agent,
                   val energy : Double,
                   val remainingBonus : Double,
                   val score : Double = 0.0,
                   val winningState : Boolean = false) extends Logging {
  
  import scala.math._
  
  // Agent mass
  private val mass = agent.constraints.mass
  
  // String representation of this state.
  private val peStateString = "State\nEnergy %s\nPosition\n%s\nVelocity\n%s".format(energy, position, velocity)
 
  /**
   * Override equals.
   * Case class version is not floating point savvy
   */
  override def equals(other : Any) = other match {
    case that : PEState => (position equals that.position) &&
                           (velocity equals that.velocity) &&
                           (agent equals that.agent) &&
                           MathHelper.doubleEquals(energy,that.energy)
    case _ => false
  }
  
  /**
   * Return true if we have zero energy and zero speed
   */
  def isActive = !(MathHelper.doubleEquals(energy, 0.0) && MathHelper.doubleEquals(velocity.magnitude, 0.0))
  
  /**
   * Generate next state from delta to heading and speed.
   */
  def nextState(deltaRotation : Double,deltaSpeed : Double) = {
    
    // Have we run out of energy?
    if (MathHelper.doubleEquals(energy, 0.0)) {
           
      if(!isActive) {
        // If we are inactive we have ground to a halt.
        // Return this PEState.
        debug("{} is inactive.\n{}", agent, this)
        this
      }
      else {
       
        // Decay velocity as friction slows us down.
        val currentSpeed = velocity.magnitude
        
        val decaySpeed = min(PhysicsHelper.decaySpeed(this),currentSpeed)
        
        val newVelocity =  velocity.unitVector.scale(currentSpeed - decaySpeed)
        
        if(!Vector.isValid(newVelocity) || newVelocity.magnitude.equals(Double.NaN)) {
          throw new IllegalStateException("Invalid Vector \n%s".format(newVelocity))
        }
        
        debug("{} is out of energy. Speed will decay by {}", agent, decaySpeed)
        
        PEState(position + newVelocity,newVelocity,agent,0.0,remainingBonus)
      }
    }
    else {
      
      val energyToAccelerate = PhysicsHelper.energyToAccelerate(agent.constraints.mass,deltaSpeed)
      
      if(energyToAccelerate > energy) {
        throw new RuntimeException("Energy to accelerate by %s (%s) is greater than remainingEnergy %s".format(deltaSpeed,energyToAccelerate,energy))
      }
      
      val finalEnergy = energy - energyToAccelerate
             
      val newVelocity = velocity.unitVector.rotate(deltaRotation).scale(velocity.magnitude + deltaSpeed)
      
      val newPosition = position + newVelocity
      
      new PEState(newPosition,newVelocity,agent,finalEnergy,remainingBonus)
    }
  }
  
  /**
   * String representation of this PEState
   */
  override def toString = peStateString
}

/**
 * PEState helper.
 */
object PEState {
  
  /**
   * Get initial state.
   */
  def initialState(agent : Agent, distanceFromOrigin : Double) = {
    
    val initialPosition = Vector.randomVectorConstMagnitude(distanceFromOrigin);
    val initialVelocity = Vector.randomVectorConstMagnitude(0.0);
                
    PEState(initialPosition,initialVelocity,agent,agent.constraints.startingEnergy,agent.constraints.winBonus);
  }
    
  /**
   * Convert translationState so to frame of reference where origin is located
   * at referenceState.position and x axis aligned with referenceState.orientation.
   */
  def toFrameOfReference(referenceState : PEState, translationState : PEState) = {
    
    // Rotation to be applied to other state to align x axes with velocity.
    val conversionAngle = -referenceState.velocity.theta
    
    // Translate translation state components
    val translationStatePosition = (translationState.position - referenceState.position) rotate conversionAngle
    val translationStateVelocity = translationState.velocity rotate conversionAngle
    
    // Build translated state.
    new PEState( translationStatePosition,translationStateVelocity,translationState.agent,translationState.energy,translationState.remainingBonus)
  }
  
  /**
   * Expand list of states given an expansion ration.
   */
  def expandStates(states : List[PEState],expansionRatio : Int) = {
   
    if(expansionRatio <= 0)
      throw new IllegalArgumentException("Expansion Ration %s must be greater than zero.".format(expansionRatio))
    
    val deltaTime = 1.0 / expansionRatio
  
    // Expand agent states.
    @tailrec def expand(inputStates : List[PEState],expandedStates : List[PEState]): List[PEState] = inputStates match {
      case List() => expandedStates
      case h :: Nil => expandedStates ++ inputStates
      case h :: t => {
        
        val startPosition = h.position
        val endState = t.head
        
        val deltaVelocity = endState.velocity.scale(deltaTime)
        val deltaEnergy = (h.energy - endState.energy) / expansionRatio
                
        // Append new set of expanded states.
        expand(t,expandedStates ++ List(h) ++ (for(time <- 1 until expansionRatio) yield PEState(startPosition + deltaVelocity.scale(time),
                                                                                      endState.velocity,
                                                                                      h.agent,
                                                                                      h.energy - (deltaEnergy * time),
                                                                                      h.remainingBonus,
                                                                                      h.score))
                                                                                      )
      }
    }
    
    expand(states,List())
  }
}