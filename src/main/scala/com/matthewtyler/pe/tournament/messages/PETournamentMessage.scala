package com.matthewtyler.pe.tournament.messages

import com.matthewtyler.pe.competition.PECompetition

/**
 * Message to pass a List of PECompetition
 */
case class PETournamentMessage(competitions : List[PECompetition],session : Int)