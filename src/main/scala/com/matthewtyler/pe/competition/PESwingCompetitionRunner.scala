package com.matthewtyler.pe.competition

import akka.actor.Actor

import scala.swing.Publisher
import scala.swing.Swing

import com.matthewtyler.pe.competition.messages.{PECompetitionMessage,PECompetitionResultMessage}
import com.matthewtyler.pe.logging.Logging

/**
 * Actor processes a competition and publishes the result on the Swing EDT.
 */
object PESwingCompetitionRunner extends Actor with Logging with Publisher {

    /**
   * preStart processing.
   */
  override def preStart = {
    info("Starting PESwingCompetitionRunner.")
  }
  
  /**
   * postStop processing.
   */
  override def postStop = {
    info("PESwingCompetitionRunner has stopped.")
  }
  
  /**
   * Receive PECompetitionMessage - publish result on Swing EDT.
   */
  def receive = { 
    // Competition Message
    case PECompetitionMessage(competition,session) => {
        
      info("Swing competition runner received PECompetitionMessage session {}", session)  
      Swing.onEDT(publish(PECompetitionResultMessage(competition.runCompetition.toList,session)))
    }
  }  
}