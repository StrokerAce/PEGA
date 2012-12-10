package com.matthewtyler.pe.command.pecommand

import com.matthewtyler.pe.state.pestate.PEState

/**
 * Command trait
 */
trait Command {

  /**
   * Evaluate Command and generate new State.
   */
  def evaluate(state : PEState,opponentState : PEState) : PEState 
}