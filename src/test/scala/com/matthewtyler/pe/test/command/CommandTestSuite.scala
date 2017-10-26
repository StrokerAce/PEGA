package com.matthewtyler.pe.test.command

import com.matthewtyler.pe.test.command.pecommand.{ForceCommandTestSuite, RotationCommandTestSuite}
import org.scalatest.Suite

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