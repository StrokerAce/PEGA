package com.matthewtyler.pe.mateable

/**
 * Trait defines interface to generate offspring
 */
trait Mateable[T] {
  
  /**
   * Generate offspring Mateable
   */
  def mate(p : T) : T
}
