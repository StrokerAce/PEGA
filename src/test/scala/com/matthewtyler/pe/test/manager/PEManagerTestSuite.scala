package com.matthewtyler.pe.test.manager

import akka.actor.ActorSystem
import akka.actor.Props

import org.scalatest.Suite
import org.scalatest.Matchers

import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.manager.PEManager
import com.matthewtyler.pe.tournament.messages.{PETournamentStartMessage,PETournamentStopMessage}
import com.matthewtyler.pe.test.constraints.TestConstraints

/**
 * PETournament test suite.
 */
class PEManagerTestSuite extends Suite with Matchers with Logging {

  /**
   * Test PETournament
   */
  def tournamentTest = {
        
    val testActorSystem = ActorSystem("TestActorSystem")
    val peManager = testActorSystem.actorOf(Props(PEManager),name = "TestPEManger")
    
    info("Starting session 1")
    peManager ! PETournamentStartMessage(TestConstraints.tournamentConstraints,
                                         TestConstraints.evaderConstraints,
                                         TestConstraints.pursuerConstraints,                                       
                                         1)
    
    Thread.sleep(15000)
    
    testActorSystem.terminate
  } 
}

/**
 * Run the test suite.
 */
object PEManagerTestSuite extends App {
  (new PEManagerTestSuite).execute()
}