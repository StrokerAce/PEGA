package com.matthewtyler.pe.manager

import scala.swing.Publisher
import scala.swing.Swing

import akka.actor.{Actor,ActorRef,Props}

import com.matthewtyler.pe.competition.messages.PECompetitionResultMessage
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.tournament._
import com.matthewtyler.pe.tournament.messages._

/**
 * PEManager object.
 * 
 * Runs a PETournament.
 * Will eventually pass events to Swing event dispatching thread to be picked up by GUI.
 * 
 * Immutable
 */
object PEManager extends Actor with Logging with Publisher {
  
  /**
   * preStart processing.
   */
  override def preStart = {
    info("Starting PEManager")
  }
  
  /**
   * postStop processing.
   */
  override def postStop = {
    info("PEManager has stopped.")
  }
  
  /**
   * Handle messages when tournament is in started state.
   */
  private def tournamentStarted(tournamentSession : Int,peTournament : ActorRef): Receive = {    
      
    case PETournamentStopMessage => {
        
      info("Stopping Tournament. Session {}.",tournamentSession)
        
      // Kill Tournament
      context.stop(peTournament)
        
      // Enter stopped state
      info("Entering tournamentStopped state.")
      
      context.unbecome
    }
      
    // Tournament result message.
    case PETournamentResultMessage(result,`tournamentSession`) => {
        
      debug("Received PETournamentResultMessage for session {}",tournamentSession)
            
      // Publish event to GUI on Swing Event Dispatch Thread
      Swing.onEDT(publish(PETournamentResultMessage(result,tournamentSession)))
 
      // Remain in started state
      context.become(tournamentStarted(tournamentSession,peTournament))
    }
          
    // Tournament complete message
    case PETournamentCompleteMessage(`tournamentSession`) => {
          
      debug("Received PETournamentComplete message for session {}.",tournamentSession)
          
      // Publish tournament complete message on EDT.
      Swing.onEDT(publish(PETournamentCompleteMessage(tournamentSession)))
                
      // Enter stopped state
      info("Entering tournamentStopped state.")
      
      context.unbecome
    } 
  }
  
  /**
   * HandleMessages when tournament is in stopped state
   */
  def receive = {
                 
    // StartMessage
    case startMessage : PETournamentStartMessage => {
        
      info("Starting new Tournament. Session {}",startMessage.session)
      
      // Create new Tournament
      val peTournament = context.actorOf(Props(new PETournament(startMessage.session,startMessage.tournamentConstraints)),name = "PETournament_%s".format(startMessage.session))
             
      peTournament ! startMessage
      
      info("Entering tournamentStarted state.")
          
      // Enter started state.
      context.become(tournamentStarted(startMessage.session,peTournament))        
    }
  }
}