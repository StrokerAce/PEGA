package com.matthewtyler.pe.tournament.messages

import scala.swing.event.Event

/**
 * PETournamentCompleteMessage class.
 * 
 * Used to notify any clients that tournament is complete.
 */
case class PETournamentCompleteMessage(session : Int) extends Event