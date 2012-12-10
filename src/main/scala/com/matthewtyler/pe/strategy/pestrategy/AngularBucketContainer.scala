package com.matthewtyler.pe.strategy.pestrategy

import scala.annotation.tailrec

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.math.MathHelper

/**
 * RadialBucketCollection
 */
class AngularBucketContainer private(val angularBuckets : List[AngularBucket]) extends Instruction with Logging with Mateable[AngularBucketContainer] {
  
  // String representation of this AngularBucketContainer
  private val stringValue = buildString
  
  /**
   * Implement Instruction trait.
   */
  def apply(myState : PEState,opponentState : PEState) = { 
    angularBuckets.find(angularBucket => angularBucket.applies(opponentState)) match {
      case None => throw new RuntimeException("AngularBucketContainer % s does not contain opponent state %s".format(this,opponentState))     
      case Some(bucket) => bucket.apply(myState,opponentState)
    }
  }
  
  /**
   * Implement Mateable trait.
   */
  def mate(partner : AngularBucketContainer) = {
      
    // Crossover bucket
    val crossoverPoint = PEStrategyHelper.crossoverAngularBucket(this)
    
    // Crossover Lists of AngularBuckets
    @tailrec def crossover(parent1 : List[AngularBucket],parent2 : List[AngularBucket],offspring : List[AngularBucket],index : Int) : List[AngularBucket] = { 
       
      debug("Processing bucket {}, crossover {}\n", index,crossoverPoint)
      // Disaster strikes
      if(index > crossoverPoint) throw new RuntimeException("Error mating AngularBucketContainers")
      // We are crossing over at this point
      if(index == crossoverPoint) (offspring.reverse) ++ ((parent1.head mate parent2.head) :: parent2.tail)
      // We haven't reached the crossover point. Keep on truckin
      else crossover(parent1.tail,parent2.tail,parent1.head :: offspring, index + 1)
    }
    
    new AngularBucketContainer(crossover(angularBuckets,partner.angularBuckets,Nil,0))
  }
  
  /**
   * Build String representing this AngularBucketContainer
   */
  private def buildString = {
    
    val title = "AngularBucketContainer\n%s"
    val pattern = "%s%s"
    
    // Append Strings.
    def build(s : String,bl : List[AngularBucket]) : String = bl match {
      case(Nil) => s
      case(h :: t) => build(pattern.format(h,s),t) 
    }

    title.format(build("",angularBuckets.reverse))
  }
  
  /**
   * String representation of RadialBucket
   */
  override def toString = stringValue
}

/**
 * Helper object
 */
object AngularBucketContainer extends Logging {

  /**
   * New random RadialBucketContainer
   */
  def newRandom(agentConstraints : AgentConstraints,tournamentConstraints : TournamentConstraints) = {
    
    val angularBucketSize = MathHelper.TWO_PI / agentConstraints.angularBuckets
      
    /**
     * Build list
     */
    @tailrec def build(remaining : Long,angle : Double,buckets : List[AngularBucket]) : List[AngularBucket] = remaining match {    
      case 0 => buckets.reverse
      case _ => build(remaining - 1, angle + angularBucketSize, new AngularBucket(agentConstraints.angularBuckets - remaining,angle, angle + angularBucketSize, RadialBucketContainer.newRandom(agentConstraints,tournamentConstraints)) :: buckets)
    }
    
    val container = new AngularBucketContainer(build(agentConstraints.angularBuckets,0.0,Nil))
    
    debug("Created new random:\n{}", container)
    
    container
  }
}
