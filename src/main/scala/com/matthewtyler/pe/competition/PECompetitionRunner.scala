package com.matthewtyler.pe.competition

import akka.actor.Actor
import com.matthewtyler.pe.competition.messages.{PECompetitionMessage, PECompetitionResultMessage}
import com.matthewtyler.pe.logging.Logging

/**
  * PECompetitionRunner
  *
  * Immutable.
  */
case class PECompetitionRunner(id: Int, tournamentSession: Int) extends Actor with Logging {

  // String representation of this PECompetitionRunner.
  private lazy val description = s"PECompetitionRunner $id"

  /**
    * preStart processing.
    */
  override def preStart = {
    info(s"Starting $description")
  }

  /**
    * postStop processing.
    */
  override def postStop = {
    info(s"$description has stopped.")
  }

  /**
    * Receive PECompetition messages, run competition and return result.
    */
  def receive = {

    // Run competition and return result
    // We will remain in this state until we are stopped by our parent.
    case PECompetitionMessage(competition, `tournamentSession`) => {

      debug(s"$description received PECompetitionMessage, Session $tournamentSession.")

      context.parent ! PECompetitionResultMessage(competition.runCompetition.toList, tournamentSession)
    }
  }
}