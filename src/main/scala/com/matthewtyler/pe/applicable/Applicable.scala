package com.matthewtyler.pe.applicable

import com.matthewtyler.pe.state.pestate.PEState

/**
 * Applicable trait.
 * 
 */
trait Applicable {
  
  /**
   * Returns true if implementor is applicable to this state.
   */
  def applies(c : PEState) : Boolean
}

