package com.matthewtyler.pe.competition.messages

import scala.actors.Actor

import com.matthewtyler.pe.competition.PECompetition

/**
 * PECompetitionMessage class.
 */
case class PECompetitionMessage(competition : PECompetition,session : Int)