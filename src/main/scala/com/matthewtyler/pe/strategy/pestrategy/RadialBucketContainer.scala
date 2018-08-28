package com.matthewtyler.pe.strategy.pestrategy

import scala.annotation.tailrec

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.instruction.Instruction
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.mateable.Mateable
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.state.pestate.PEState
import com.matthewtyler.pe.strategy.tactic.PETactic

/**
 * RadialBucketCollection
 */
class RadialBucketContainer private(val radialBuckets : List[RadialBucket],
                                    val longRangeTactic : PETactic,
                                    val radialBucketMaxSize : Int,
                                    tournamentConstraints : TournamentConstraints) extends Instruction with Logging with Mateable[RadialBucketContainer] {
  
  // String representation of this RadialBucketContainer
  private val stringValue = buildString
  
  /**
   * Implement Instruction trait.
   */
  def applyInstruction(myState : PEState,opponentState : PEState) = {
    
    // Check to see if opponent state lies beyond radialBucketList range.
    // If so apply longRangeInstruction
    if(radialBuckets.last.radiusMax >= opponentState.position.theta) {
      longRangeTactic.applyInstruction(myState,opponentState)
    }
    else {
      radialBuckets.find(radialBucket => radialBucket.applies(opponentState))
        .map(_.applyInstruction(myState,opponentState))
        .getOrElse(longRangeTactic.applyInstruction(myState,opponentState))
    }
  }
  
  /**
   * Implement Mateable trait.
   */
  def mate(partner : RadialBucketContainer) = {
    
    val crossoverPoint = PEStrategyHelper.crossoverRadialBucket(this)
    
    // Crossover list of RadialBuckets
    @tailrec def crossover(parent1 : List[RadialBucket],parent2 : List[RadialBucket],offspring : List[RadialBucket],index : Int, cumulativeRadius : Int) : List[RadialBucket] = parent1 match {
      
      case Nil => return offspring.reverse
      case h :: t => {  
        // Determine the next RadialBucket to add.
        val nextBucket = if(index == crossoverPoint ) parent1.head mate parent2.head // Generate offspring 
                         else {
                           // Use p1 head.
                           if(index < crossoverPoint) parent1.head
                           // Use p2 head asjusted for min radius
                           else new RadialBucket(index,cumulativeRadius,cumulativeRadius + parent2.head.size,parent2.head.tactic,tournamentConstraints) 
                         }
        
        crossover(parent1.tail,parent2.tail,nextBucket :: offspring, index + 1,cumulativeRadius + nextBucket.size)
      }
    }
    
    // Generate new RadialBucketContainer from crossover of RadialBucket lists.
    new RadialBucketContainer(crossover(radialBuckets,partner.radialBuckets,Nil,0,0),longRangeTactic mate partner.longRangeTactic,radialBucketMaxSize,tournamentConstraints)
  }
  
  /**
   * Build String representing of this RadialBucketContainer
   */
  private def buildString = {
    
    val title = "RadialBucketContainer\n%s"
    val pattern = "%s%s"
    
    // Append Strings.
    @tailrec def build(s : String, bl : List[RadialBucket]) : String = bl match {
      case Nil => s
      case h :: t => build(pattern.format(h,s),t) 
    }
 
    "%s\nLong Range Tactic : %s\n".format(title.format(build("",radialBuckets.reverse)),longRangeTactic)
  }
  
  /**
   * String representation of RadialBucket
   */
  override def toString = stringValue
}

/**
 * Helper object
 */
object RadialBucketContainer extends Logging {

  /**
   * New random RadialBucketContainer
   */
  def newRandom(agentConstraints : AgentConstraints,tournamentConstraints : TournamentConstraints) = {
     
    // Build Radial Bucket list.
    @tailrec def build(remaining : Long, radius : Int, buckets : List[RadialBucket]) : List[RadialBucket] = remaining match {    
      case 0 => buckets.reverse
      case _ => {
        val size = RandomFactory.nextIntInRange(agentConstraints.maxRadialBucketSize)
        build(remaining - 1,radius + size,new RadialBucket(agentConstraints.radialBuckets - remaining,radius,radius + size,PETactic.newRandom(tournamentConstraints),tournamentConstraints) :: buckets)
      }
    }
    
    val container = new RadialBucketContainer(build(agentConstraints.radialBuckets,0,Nil),PETactic.newRandom(tournamentConstraints),agentConstraints.maxRadialBucketSize,tournamentConstraints)
    
    debug("Created new RadialBucketContainer:\n{}",container)
    
    container  
  }
}