package com.matthewtyler.pe.competition.messages

import scala.swing.event.Event

import com.matthewtyler.pe.competition.PECompetitionResult

/**
 * PECompetitionResultMessage
 */
case class PECompetitionResultMessage(result : List[PECompetitionResult],session : Int) extends Event