package com.matthewtyler.pe.strategy.pestrategy

import scala.annotation.tailrec

import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.random.RandomFactory

/**
 * Helper object for PEStrategy
 * Will eventually configure from config/GUI.
 */
object PEStrategyHelper {
    
  /**
   * Get random crossover AngularBucket index.
   */
  def crossoverAngularBucket(angularBucketContainer : AngularBucketContainer) = RandomFactory.randomGenerator.nextInt(angularBucketContainer.angularBuckets.size)
  
  /**
   * Get random crossover RadialBucket index.
   */
  def crossoverRadialBucket(radialBucketContainer : RadialBucketContainer) = RandomFactory.randomGenerator.nextInt(radialBucketContainer.radialBuckets.size)
    
  /**
   * Returns true is AngularBucketContainer is valid.
   * 
   * Used in Unit tests.
   */
  def validateAngularBucketContainer(angularBucketContainer : AngularBucketContainer) = {
    
    val angularBucketSize = MathHelper.TWO_PI / angularBucketContainer.angularBuckets.size
    
    // Validation function
    @tailrec def validate(angularBuckets : List[AngularBucket],expectedIndex : Int ) : Boolean = angularBuckets match {
      // Validate bucket
      
      // We have fallen off the end of the list because the last bucket did not have expected upper value. 
      case Nil => false
      // Still buckets to validate.
      case h :: t if(validateRadialBucketContainer(h.radialBuckets) & h.index == expectedIndex && MathHelper.doubleEquals(h.thetaMax - h.thetaMin,angularBucketSize)) => { 
        t match {
          // If last bucket validate upper bound is 2Pi
          case Nil if(MathHelper.doubleEquals(h.thetaMax,MathHelper.TWO_PI)) => true
          case _ => validate(t,expectedIndex + 1)
        }
      }
      
      case _ => false
      
    }
    
    validate(angularBucketContainer.angularBuckets,0)
  }
  
  /**
   * Returns true if RadialBucketContainer is valid.
   */
  def validateRadialBucketContainer(radialBucketContainer : RadialBucketContainer) = {
    
    val radialBucketMaxSize = radialBucketContainer.radialBucketMaxSize
    
    // Validation function
    @tailrec def validate(radialBuckets : List[RadialBucket],cumulativeRadius : Int,expectedIndex : Int) : Boolean = radialBuckets match {
      case Nil => true
      case h :: t if(h.index == expectedIndex && 
                     h.radiusMin == cumulativeRadius && 
                     (h.radiusMax - h.radiusMin) == h.size 
                     && h.size <= radialBucketMaxSize) => validate(t,cumulativeRadius + h.size, expectedIndex + 1)
      case _ => false
    }
    
    validate(radialBucketContainer.radialBuckets,0,0)
  }
}

