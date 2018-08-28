package com.matthewtyler.pe.instruction

import com.matthewtyler.pe.state.pestate.PEState

/**
  * Instruction trait.
  */
trait Instruction {

  /**
    * Apply instruction.
    */
  def applyInstruction(myState: PEState, opponentState: PEState): PEState
}
