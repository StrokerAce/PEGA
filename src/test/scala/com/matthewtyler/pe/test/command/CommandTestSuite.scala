package com.matthewtyler.pe.test.command

import org.scalatest.Suite
import org.scalatest.matchers.MustMatchers

import com.matthewtyler.pe.math.MathHelper
import com.matthewtyler.pe.test.command.pecommand.{ForceCommandTestSuite,RotationCommandTestSuite}

/**
 * Command test suite.
 */
class CommandTestSuite extends Suite {

  (new RotationCommandTestSuite).execute()
  (new ForceCommandTestSuite).execute()
}

/**
 * Run the test suite.
 */
object CommandTestSuite extends App {
  (new CommandTestSuite).execute()
}