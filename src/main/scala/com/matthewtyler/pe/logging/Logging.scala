package com.matthewtyler.pe.logging

import org.slf4j.LoggerFactory

/**
  * Logging trait which wraps sl4j
  *
  * I will eventually configure logging from a file but for now lets concentrate
  * on the more interesting things.
  */
trait Logging {

  // Logger instance
  private val log = LoggerFactory.getLogger(getClass)

  // Trace
  def trace(message: String, values: Any*) = log.trace(message, values.map(_.asInstanceOf[Object]).toArray)

  def trace(message: String, error: Throwable) = log.trace(message, error)

  // Debug
  def debug(message: String, values: Any*) = log.debug(message, values.map(_.asInstanceOf[Object]).toArray)

  def debug(message: String, error: Throwable) = log.debug(message, error)

  // Info
  def info(message: String, values: Any*) = log.info(message, values.map(_.asInstanceOf[Object]).toArray)

  def info(message: String, error: Throwable) = log.info(message, error)

  // Warn
  def warn(message: String, values: Any*) = log.warn(message, values.map(_.asInstanceOf[Object]).toArray)

  def warn(message: String, error: Throwable) = log.warn(message, error)

  // Error
  def error(message: String, values: Any*) = log.error(message, values.map(_.asInstanceOf[Object]).toArray)

  def error(message: String, error: Throwable) = log.error(message, error)
}
