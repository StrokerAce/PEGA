package com.matthewtyler.pe.tournament.messages

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}

/**
 * Start Tournament message.
 */
case class PETournamentStartMessage(tournamentConstraints : TournamentConstraints,
                                    evaderConstraints : AgentConstraints,
                                    pursuerConstraints : AgentConstraints,
                                    session : Int = 0)