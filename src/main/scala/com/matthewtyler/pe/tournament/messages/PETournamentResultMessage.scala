package com.matthewtyler.pe.tournament.messages

import scala.swing.event.Event

import com.matthewtyler.pe.competition.PECompetitionResult

/**
 * PETournamentResultMessage
 */
case class PETournamentResultMessage(result : List[List[PECompetitionResult]],session : Int) extends Event