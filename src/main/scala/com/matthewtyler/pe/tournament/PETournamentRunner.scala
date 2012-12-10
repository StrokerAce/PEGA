

package com.matthewtyler.pe.tournament


import akka.actor.{Actor,ActorRef,Props}

import com.matthewtyler.pe.competition._
import com.matthewtyler.pe.competition.messages.{PECompetitionMessage,PECompetitionResultMessage}
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.tournament.messages._

/**
 * PETornamentRunner Actor.
 * 
 * Partitions competitions to PECompetition Actors and sends result of tournament to manager.
 */
case class PETournamentRunner(private val tournamentSession : Int,
                              private val competitionRunnerInstances : Int = 10) extends Actor with Logging {
   
  /**
   * preStart processing.
   */
  override def preStart = {
    info("Starting PETournamentRunner session %s".format(tournamentSession))
          
    // Create PECompetitionRunners
    for(i <- 1 to competitionRunnerInstances) 
      context.actorOf(Props(PECompetitionRunner(i,tournamentSession)),name = "CompetitionRunner_%s".format(i))
  }
  
  /**
   * postStop processing.
   */
  override def postStop = {
    info("PETournamentRunner %s has stopped.".format(tournamentSession))
  }
  
  /**
   * Tournament running state.
   */
  private def tournamentRunning(remainingResults : Int,competitionResults : List[List[PECompetitionResult]]) : Receive = {
                   
    // Received competition result message and all results are not yet in!
    // Process Competition Result.
    case PECompetitionResultMessage(result,`tournamentSession`) => {
        
      val newResults = result :: competitionResults
        
      val outstandingResults = remainingResults - 1
        
      debug("Received PECompetitionResultMessage. Outstanding competitions %s".format(outstandingResults))
              
      // if we have results for all tournaments notify manager.
      if(outstandingResults == 0) {
        debug("Tournament complete")
        context.parent ! PETournamentResultMessage(newResults,tournamentSession)
          
        // Enter tournament started state.
        debug("Entering tournamentStarted state.")
        context.become(tournamentStarted)
      }
      else {
          // Remain in tournament running state.
          context.become(tournamentRunning(outstandingResults,newResults))
      }
    }
  }
  
  /**
   * Tournament Started state.
   */
  private def tournamentStarted : Receive = {    
       
    // Received a new set of competitions.
    // Run new set of competitions
    case PETournamentMessage(competitions,`tournamentSession`) => {
        
      debug("Received PETournamentMessage for %s competitions".format(competitions.size))
               
      // Partition competitions between runners for execution.  
      def runCompetitions(remainingCompetitions : List[PECompetition],runners : List[ActorRef]): Unit = remainingCompetitions match {
            
        // All competitions run 
        case Nil => return
            
        // Partition competitions between competition runners
        case headCompetition :: tailCompetitions => runners match {
            
          // We have sent a competition to each runner.
          // Start again.
          case Nil => {
            context.children.head ! PECompetitionMessage(headCompetition,tournamentSession)
            runCompetitions(tailCompetitions,context.children.toList.tail)
          }
          case headRunner :: tailRunners => {
            headRunner ! PECompetitionMessage(headCompetition,tournamentSession)
            runCompetitions(tailCompetitions,tailRunners)
          }
        }
      }
          
      // Distribute competitions between runners.
      runCompetitions(competitions,context.children.toList)
      
      debug("Entering tournamentRunning state.")
      
      // Enter tournamentRunning state
      context.become(tournamentRunning(competitions.size,List[List[PECompetitionResult]]()))
    }        
  }
   
  /**
   * Receive method
   */
  def receive = {
       
    case startMessage : PETournamentStartMessage => {
        
      debug("Received PETournamentStartMessage, session {}.",startMessage.session)       
      debug("Entering tournamentStarted state.")
      
      // Enter started state
      context.become(tournamentStarted)
    }
  }
}