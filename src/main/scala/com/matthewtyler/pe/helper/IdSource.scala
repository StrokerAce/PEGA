package com.matthewtyler.pe.helper

/**
  * Source of ID numbers.
  */
trait IdSource {

  var id: Long = 0

  /**
    * Get next Id
    */
  def nextId: Long = synchronized {
    id += 1
    id
  }
}
