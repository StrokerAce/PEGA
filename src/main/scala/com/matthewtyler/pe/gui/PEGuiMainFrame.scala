package com.matthewtyler.pe.gui

import java.awt.Color

import javax.swing.border.{LineBorder,TitledBorder}

import scala.collection.mutable.HashMap
import scala.swing._
import scala.swing.event._
import scala.swing.Publisher
import scala.swing.TabbedPane._
import scala.swing.ListView.IntervalMode

import akka.actor.{ActorSystem,Props}
import com.typesafe.config.ConfigFactory

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.manager.PEManager
import com.matthewtyler.pe.tournament._
import com.matthewtyler.pe.tournament.messages._

/**
 * This will eventually be the Main frame of the GA GUI and enable
 * user to set constraints and view/replay competitions.
 */
object PEGuiMainFrame extends SwingApplication with Logging with Publisher {

  /**
   * Implement Top method
   */
  def top = new MainFrame {
    
    title = "Pursuer Evader Genetic Algorithm"
      
    /*
     * Tournament session.
     */
    var session = 0
    
    /*
     * Tournament generation
     */
    var generation = 0
    
    /*
     * Actor System configuration.
     * 
     * Limit the PEGAActorSystem threads to 1 less than the number of available CPUs
     * or 1 if we are on a single CPU machine.
     */
    val customConf = ConfigFactory.parseString("""
      akka{
        log-config-on-start = on
        
        actor.default-dispatcher.fork-join-executor {
          parallelism-min = 1
          parallelism-factor = 1.0
          parallelism-max = %s
  }
      }""".format(scala.math.max(Runtime.getRuntime.availableProcessors - 1,1)))
    
    val actorSystem = ActorSystem("PegaActorSystem",ConfigFactory.load(customConf))
    
    /**
     * PEManager Actor
     */
    val peManager = actorSystem.actorOf(Props(PEManager),name = "PEManager")
               
    /**
     * Output text fields.
     */
    val generationField = new TextField {
      text = "0"
      background = Color.WHITE
      editable = false
      columns = 10
      horizontalAlignment = Alignment.Right
    }
    
    val averageEvaderFitnessField = new TextField {
      text = "0"
      background = Color.WHITE
      editable = false
      columns = 10
      horizontalAlignment = Alignment.Right
    }
    
    val evaderEscapeEventsField = new TextField {
      text = "0"
      background = Color.WHITE
      editable = false
      columns = 10
      horizontalAlignment = Alignment.Right
    }
    
    val averagePursuerFitnessField = new TextField {
      text = "0"
      background = Color.WHITE
      editable = false
      columns = 10
      horizontalAlignment = Alignment.Right
    }
    
    val pursuerCaptureEventsField = new TextField {
      text = "0"
      background = Color.WHITE
      editable = false
      columns = 10
      horizontalAlignment = Alignment.Right
    }
    
    val outputFields = List(generationField,averageEvaderFitnessField,averagePursuerFitnessField,evaderEscapeEventsField,pursuerCaptureEventsField)
    
    /**
     * Algorithm buttons.
     */
    val startAlgorithmButton = new Button {
      text = "Start Algorithm"
      enabled = true
    }
    
    val stopAlgorithmButton = new Button {
      text = "Stop Algorithm"
      enabled = false
    }
    
    /*
     * PEWinPanel
     */
    val peWinPanel = new PEWinPanel
                    
    /**
     * Input panel 
     */
    val inputPanel = new BoxPanel(Orientation.Vertical) {
                    
      contents += new GridPanel(6,2) {
        
        hGap = 5
        vGap = 5
        
        border = Swing.TitledBorder(Swing.LineBorder(Color.BLACK),"Algorithm Status")

        contents += startAlgorithmButton
        
        contents += stopAlgorithmButton
        
        contents += new Label{
          text = "Generation"
          xAlignment = Alignment.Left
        }
      
        contents += generationField
      
        contents += new Label{
          text = "Av. Evader fitness"
          xAlignment = Alignment.Left
        }
      
        contents += averageEvaderFitnessField
      
        contents += new Label{
          text = "Evader escape events"
          xAlignment = Alignment.Left
        }
      
        contents += evaderEscapeEventsField

        contents += new Label{
          text = "Av. Pursuer fitness"
          xAlignment = Alignment.Left
        }
      
        contents += averagePursuerFitnessField
      
        contents += new Label{
          text = "Pursuer capture events"
          xAlignment = Alignment.Left
        }
      
        contents += pursuerCaptureEventsField
      }
      
      contents += new BoxPanel(Orientation.Vertical) {
      
        border = new TitledBorder(new LineBorder(Color.BLACK),"Win statistics")
        
        contents += peWinPanel
      }
    }
       
    /*
     * Statistics panel.
     */
    val evaderStatsPanel = new StatsPanel("Evader",Color.BLUE)
    val pursuerStatsPanel = new StatsPanel("Pursuer",Color.RED)
    
    val statisticsPanel = new BoxPanel(Orientation.Vertical) {
      
      border = new TitledBorder(new LineBorder(Color.BLACK),"Algorithm statistics")
      
      contents += evaderStatsPanel
      contents += pursuerStatsPanel
    }
    
    contents = new TabbedPane {
      
      pages +=  new Page("Algorithm",new BoxPanel(Orientation.Horizontal) {
        contents += inputPanel
        contents += statisticsPanel
      })
      
      pages += new Page("Agent Competitions",AgentCompetitionPanel)
      
      pages += new Page("Configuration",ConstraintsPanel )
    }
        
    /*
     * Register events.
     */
    listenTo(startAlgorithmButton)
    listenTo(stopAlgorithmButton)
    listenTo(PEManager)
       
    /*
     * Process events.
     */
    reactions += {
      
      // Start button pressed.
      case ButtonClicked(`startAlgorithmButton`) => {
        
        try {
          toggleStartStopEnabled
          GuiHelper.updateFields(outputFields,_.text = "0")
          processInputAndStartAlgorithm
          ConstraintsPanel.setEnabled(false)
          evaderStatsPanel.reset
          pursuerStatsPanel.reset
          peWinPanel.reset
          AgentCompetitionPanel.reset                 
        }
        catch {
              
          case e => {
            error(e.getMessage())
            GuiHelper.updateFields(outputFields,_.text = "0")
            toggleStartStopEnabled
          }        
        }
      }
      
      // Stop button pressed.
      case ButtonClicked(`stopAlgorithmButton`) => {
        info("Stopping Algorithm.")
        toggleStartStopEnabled
        ConstraintsPanel.setEnabled(true)
        
        // Send stop message to manager.
        peManager ! PETournamentStopMessage
      }
                 
      // Tournament result.
      case tournamentResult : PETournamentResultMessage => {
               
        info("Tournament result.")
        
        generationField.text = generation.toString
                
        val summary = PETournamentResultSummary.summarise(generation,tournamentResult.result)
              
        generation += 1
        
        val evaderFitness = (summary.cumEvaderScore / summary.competitions)
        val pursuerFitness = (summary.cumPursuerScore / summary.competitions)
        
        averageEvaderFitnessField.text = evaderFitness.round.toString
        averagePursuerFitnessField.text = pursuerFitness.round.toString
        
        evaderEscapeEventsField.text = summary.evaderEscapeEvents.toString
        pursuerCaptureEventsField.text = summary.pursuerCaptureEvents.toString
        
        evaderStatsPanel.update(evaderFitness)
        pursuerStatsPanel.update(pursuerFitness)
        
        peWinPanel.update(summary.evaderEscapeEvents,summary.pursuerCaptureEvents)
        
        // Publish summary
        PEGuiMainFrame.publish(summary)
      }
      
      // Tournament complete
      case tournamentComplete : PETournamentCompleteMessage => {
        info("Tournament complete!")
        toggleStartStopEnabled
        ConstraintsPanel.setEnabled(true)
      }
    }
    
    /**
     * Get next session Id.
     */
    private def nextSession = {
      session += 1
      
      session
    }
        
    /**
     * Process user input and start algorithm.
     */
    private def processInputAndStartAlgorithm = {
      
      generation = 1
      
      // Parse text field value to Int.
      def parseInput(field : TextField): Int = {
       
        // Local value to parse input.
        val intValue = Integer.parseInt(field.text)
        
        if(intValue < 1)
          throw new IllegalArgumentException("Error parsing text field. %s must be >= 1.".format(intValue));
        
        intValue
      }
      
      // Send start message to manager.
      peManager ! PETournamentStartMessage(ConstraintsPanel.tournamentConstraints,
                                           ConstraintsPanel.evaderConstraints,
                                           ConstraintsPanel.pursuerConstraints,
                                           nextSession)
    }
        
    /**
     * Toggle enabled state of start/stop algorithm buttons.
     */
    private def toggleStartStopEnabled = {
      startAlgorithmButton.enabled = !startAlgorithmButton.enabled
      stopAlgorithmButton.enabled = !stopAlgorithmButton.enabled
    }
  }
        
  /**
   * Implement startup method
   */
  def startup(args: Array[String]) = {
    
    info("Started GUI.")
    
    top.open()
  }
  
  /**
   * Implement quit method.
   * 
   * Call to gracefully exit GUI.
   */
  override def quit() = {
    // Shutdown actor system.
    top.actorSystem.shutdown
    info("Exited GUI.")
  }
}