package com.matthewtyler.pe.tournament

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

import scala.swing.event.Event

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.competition.PECompetitionResult

/**
 * PETournamentResultSummary
 */
case class PETournamentResultSummary(generation           : Int,
                                     competitions         : Int,
                                     cumEvaderScore       : Double,
                                     cumPursuerScore      : Double,
                                     evaderEscapeEvents   : Int,
                                     pursuerCaptureEvents : Int,
                                     evaders              : TreeSet[Agent],
                                     pursuers             : TreeSet[Agent],
                                     evaderFitness        : Map[Agent,Double],
                                     pursuerFitness       : Map[Agent,Double]) extends Event

/**
 * PETournamentResultSummary factory.
 */
object PETournamentResultSummary {
  
  /**
   * Summarise tournament results
   */
  def summarise(generation : Int,competitionResults : List[List[PECompetitionResult]]) = {
    
    @tailrec def process(results : List[PECompetitionResult],size : Int,summary : PETournamentResultSummary): PETournamentResultSummary = results match {
      
      case Nil => summary
      case h :: t => {
        
        val evader = h.evaderStates.head.agent
        val pursuer = h.pursuerStates.head.agent
        
        val newSummary = PETournamentResultSummary(generation,
                                                   size,
                                                   summary.cumEvaderScore + h.evaderResult.averageScore,
                                                   summary.cumPursuerScore + h.pursuerResult.averageScore,
                                                   summary.evaderEscapeEvents + h.evaderResult.bonusEvents,
                                                   summary.pursuerCaptureEvents + h.pursuerResult.bonusEvents,
                                                   if(!(summary.evaders contains evader)) summary.evaders + evader else summary.evaders,
                                                   if(!(summary.pursuers contains pursuer)) summary.pursuers + pursuer else summary.pursuers,
                                                   summary.evaderFitness + ((evader,summary.evaderFitness.getOrElse(evader,0.0) + h.evaderResult.averageScore)),
                                                   summary.pursuerFitness + ((pursuer,summary.pursuerFitness.getOrElse(pursuer,0.0) + h.pursuerResult.averageScore))
                                                   )     
        process(t,size+1,newSummary)
        
      }
    }
    
    process(competitionResults.flatten,1,PETournamentResultSummary(generation,0,0.0,0.0,0,0,TreeSet[Agent](),TreeSet[Agent](),Map[Agent,Double](),Map[Agent,Double]())) 
  }  
}