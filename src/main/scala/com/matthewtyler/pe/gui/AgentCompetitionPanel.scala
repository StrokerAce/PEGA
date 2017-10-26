package com.matthewtyler.pe.gui

import java.awt.Color

import javax.swing.border.LineBorder
import javax.swing.border.TitledBorder

import scala.collection.mutable.HashMap

import scala.swing._
import scala.swing.event.{ButtonClicked,ListSelectionChanged}
import scala.swing.ListView.IntervalMode

import akka.actor.{ActorSystem,Props}

import com.matthewtyler.pe.agent.{Agent,AgentScore}
import com.matthewtyler.pe.competition._
import com.matthewtyler.pe.competition.messages.{PECompetitionMessage,PECompetitionResultMessage}
import com.matthewtyler.pe.gui.messages._
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.tournament.PETournamentResultSummary

/**
 * Competition specific details.
 */
object AgentCompetitionPanel extends BorderPanel with Logging {
  
  /*
   * Actor initialisation
   */
  val actorSystem = ActorSystem("PEGASwingActorSystem")
  val swingCompetitionRunner = actorSystem.actorOf(Props(new PESwingCompetitionRunner),name = "PESwingCompetitionRunner")
  
  //Agent/fitness pairs.
  val evaders = HashMap[Int,List[AgentScore]]()
  val pursuers = HashMap[Int,List[AgentScore]]()
  
  /*
   * Competition session 
   */
  var competitionSession = 0
  
  var competitionResult = PECompetitionResult(List(),List(),0,0,0,0)
  
  /**
   * Generation/Agent selection panel.
   */
  class AgentPanel(title : String,
                   generationsListView : ListView[Int],
                   agentsListView : ListView[AgentScore]
                   ) extends GridPanel(2,1) {
    
    border = new TitledBorder(new LineBorder(Color.BLACK),title)
    
    hGap =5
    vGap = 5
                 
    /*
     * Agent generations
     */
    contents += new BoxPanel(Orientation.Vertical) { 
      
      contents += new Label{
        text = "Generations"
        xAlignment = Alignment.Left
      }
      
      contents += new ScrollPane(generationsListView)
    }
    
    /*
     * Agent generations
     */
    contents += new BoxPanel(Orientation.Vertical) { 
      
      contents += new Label{
        text = "Agents"
        xAlignment = Alignment.Left
      }
      
      contents += new ScrollPane(agentsListView)
    }
  }
  
  /**
   * Selected Agent panel.
   */
  class SelectionPanel extends GridPanel(1,4) {
    
    border = new TitledBorder(new LineBorder(Color.BLACK),"Selected Agents for competition")
    
    hGap =5
    vGap = 5
    
    contents += new Label{
        text = "Evader generation"
        xAlignment = Alignment.Left
    }
    
    contents += selectedEvaderGenerationField
        
    contents += new Label{
        text = "Evader fitness"
        xAlignment = Alignment.Left
    }
    
    contents += selectedEvaderFitnessField
    
    contents += new Label{
        text = "Evader"
        xAlignment = Alignment.Left
    }
    
    contents += selectedEvaderField
    
    contents += new Label{
        text = "Pursuer generation"
        xAlignment = Alignment.Left
    }
    
    contents += selectedPursuerGenerationField  
   
    contents += new Label{
        text = "Pursuer fitness"
        xAlignment = Alignment.Left
    }
    
    contents += selectedPursuerFitnessField 
    
    contents += new Label{
        text = "Pursuer"
        xAlignment = Alignment.Left
    }
    
    contents += selectedPursuerField
  }
  
  /*
   * Competition controls panel
   */
  object ControlPanel extends GridPanel(1,6) {
    
    border = new TitledBorder(new LineBorder(Color.BLACK),"Competition controls")
    
    hGap =5
    vGap = 5    
        
    contents += runCompetitionButton
    contents += replayCompetitionButton    
    contents += stopCompetitionButton
  }
  
  /*
   * Generation ListViews
   */
  val evaderGenerationListView = new ListView[Int] {
    selection.intervalMode = IntervalMode.Single
    fixedCellWidth = 50
  }
    
  val pursuerGenerationListView = new ListView[Int] {
    selection.intervalMode = IntervalMode.Single
    fixedCellWidth = 50
  }
    
  /*
   * Generation list views
   */
  val generationListViews = List(evaderGenerationListView,pursuerGenerationListView)
    
  /*
   * Agent list views
   */
  val evadersListView = new ListView[AgentScore] {
    selection.intervalMode = IntervalMode.Single
    fixedCellWidth = 50
  }
    
  val pursuersListView = new ListView[AgentScore] {
    selection.intervalMode = IntervalMode.Single
    fixedCellWidth = 50
  }
    
  val agentListViews = List(evadersListView,pursuersListView)  
  
  /*
   * Selected Agent text fields
   */
  val selectedEvaderField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
    
  val selectedPursuerField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
 
  val selectedEvaderFitnessField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
    
  val selectedPursuerFitnessField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
  
  val selectedEvaderGenerationField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
    
  val selectedPursuerGenerationField = new TextField {
    text = ""
    editable = false
    horizontalAlignment = Alignment.Right
    background = Color.LIGHT_GRAY
  }
  
  val agentTextFields = List(selectedEvaderField,
                             selectedPursuerField,
                             selectedEvaderFitnessField,
                             selectedPursuerFitnessField,
                             selectedEvaderGenerationField,
                             selectedPursuerGenerationField)
  
  /*
   * Selected agents
   */
  var evader : Option[Agent] = None
  var pursuer : Option[Agent] = None
  
  var evaderFitness : Option[String] = None
  var pursuerFitness : Option[String] = None
  
  /*
   * Format fitness to 2dp.
   */
  def fitnessFormatter(fitness : Double) = "%.2f".format(fitness)
  
  var evaderGeneration : Option[Int] = None
  var pursuerGeneration : Option[Int] = None
  
  /*
   * Competition control buttons.
   */
  val runCompetitionButton = new Button {
    text = "Run new Competition"
    enabled = false
  }
  
  val replayCompetitionButton = new Button {
    text = "Replay Competition"
    enabled = false
  }
  
  val stopCompetitionButton = new Button {
    text = "Stop Competition"
    enabled = false
  }
  
  /*
   * Panels
   */
  val evaderPanel = new AgentPanel("Evaders",evaderGenerationListView,evadersListView)
  val pursuerPanel = new AgentPanel("Pursuers",pursuerGenerationListView,pursuersListView)
  val selectedAgentsPanel = new SelectionPanel 
  val controlPanel = ControlPanel
  
  import BorderPanel.Position._
  
  /*
   * Layout panels 
   */
  layout(evaderPanel) = West
  layout(pursuerPanel) = East
  layout(selectedAgentsPanel) = North
  layout(controlPanel) = South
  layout(AgentCompetitionCanvas) = Center
  
  /*
   * Register events.
   */
  listenTo(PEGuiMainFrame)
  listenTo(evaderGenerationListView.selection)
  listenTo(pursuerGenerationListView.selection)
  listenTo(evadersListView.selection)
  listenTo(pursuersListView.selection)
  listenTo(runCompetitionButton)
  listenTo(replayCompetitionButton)
  listenTo(stopCompetitionButton)
  listenTo(PESwingCompetitionRunner)
  listenTo(AgentCompetitionCanvas)
  
  /*
   * Process events
   */
  reactions += {
    
    //Process tournament result message.
    case summary : PETournamentResultSummary => {
      
      // Update generation list views
      GuiHelper.addToListViews(generationListViews,summary.generation)
      
      val evaderScores = for((agent,fitness) <- summary.evaderFitness) yield AgentScore(agent,fitness)
      val pursuerScores = for((agent,fitness) <- summary.pursuerFitness) yield AgentScore(agent,fitness)
                 
      // Update agent populations.
      evaders.put(summary.generation,evaderScores.toList.sortWith(AgentScore.compare))
      pursuers.put(summary.generation,pursuerScores.toList.sortWith(AgentScore.compare))   
    }
    
    // Evader generation selected
    case ListSelectionChanged(`evaderGenerationListView`,_,true) if evaderGenerationListView.selection.items.size > 0 =>  {
        
      val generation = evaderGenerationListView.selection.items.head
        
      debug("Selected Evader Generation {}",generation)
      
      evaderGeneration = Some(generation)
      
      // Get evaders for this generation and populate evadersListview
      evadersListView.listData = evaders.get(generation) match {
        case None => throw new IllegalStateException("Evaders does not contain generation %s".format(generation))
        case Some(evaders) => {
          
          // Wipe selected evader
          evader = None           
          evaders
        }
      }
      
      // Process selection change
      agentSelectionChanged
    }
      
    // Evader Selected
    case ListSelectionChanged(`evadersListView`,_,true) if evadersListView.selection.items.size > 0 => {       
        
      val selectedEvader = evadersListView.selection.items.head
        
      info("Selected Evader {}",selectedEvader.agent)
        
      // Populate evader selection
      evader = Some(selectedEvader.agent)
      evaderFitness = Some(fitnessFormatter(selectedEvader.score))
      
      // Process selection change
      agentSelectionChanged
    }

    // Pursuer generation selected
    case ListSelectionChanged(`pursuerGenerationListView`,_,true) if pursuerGenerationListView.selection.items.size > 0  =>  {
        
      val generation = pursuerGenerationListView.selection.items.head
       
      debug("Selected Pursuer Generation {}",generation)
      
      pursuerGeneration = Some(generation)
      
      // Get pursuers for this generation and populate pursuersListview
      pursuersListView.listData = pursuers.get(generation) match {
        case None => throw new IllegalStateException("Pursuers does not contain generation %s".format(generation))
        case Some(pursuers) => {
            
          // Wipe selected pursuer
          pursuer = None
          pursuers
        }    
      }
      
      // Process selection change
      agentSelectionChanged
    }
    
    // Pursuer selected
    case ListSelectionChanged(`pursuersListView`,_,true) if pursuersListView.selection.items.size > 0 => {
        
      val selectedPursuer = pursuersListView.selection.items.head
        
      debug("Selected Pursuer {}",selectedPursuer.agent)
        
      pursuer = Some(selectedPursuer.agent)
      pursuerFitness = Some(fitnessFormatter(selectedPursuer.score))
        
      // Process selection change
      agentSelectionChanged
    }
    
    // Run competition button pressed.
    case ButtonClicked(`runCompetitionButton`) => {
      
      debug("Run competition button clicked.")
      
      val selectedEvader = evader.getOrElse(throw new IllegalStateException("Evader has not been selected."))
      val selectedPursuer = pursuer.getOrElse(throw new IllegalStateException("Pursuer has not been selected."))
        
      debug("Running competition between {} and {}",selectedEvader,selectedPursuer)
      
      competitionSession += 1
     
      // Send competition to Swing competition runner to be run.
      swingCompetitionRunner ! PECompetitionMessage(PECompetition(selectedEvader,selectedPursuer,ConstraintsPanel.tournamentConstraints,true),competitionSession)
     
      enableAgentSelection(false)
      enableCompetitionButtons(false,false,false)
    }
    
    // Replay competition button pressed
    case ButtonClicked(`replayCompetitionButton`) => {
      debug("Replay competition button clicked")
            
      enableAgentSelection(false)
      enableCompetitionButtons(false,false,true)
      
      AgentCompetitionCanvas.start(competitionResult)
    }
    
    // Stop competition button pressed.
    case ButtonClicked(`stopCompetitionButton`) => {
      debug("Stop competition button clicked")
      
      enableAgentSelection(true)   
      enableCompetitionButtons(true,true,false)
      
      AgentCompetitionCanvas.stop
    }
    
    // Competition result message received.
    case resultMessage : PECompetitionResultMessage => {
      
      val session = resultMessage.session
      competitionResult = resultMessage.result.head
      
      if(resultMessage.session == competitionSession) {
        debug("Received PECompetitionResultmessage for session {}",session)
              
        enableAgentSelection(false)
        enableCompetitionButtons(false,false,true)
        AgentCompetitionCanvas.start(competitionResult)
      }
      else {
        debug("Ignoring PECompetitionResultMessage for stale session {}",session)
      }
    }
    
    // Replay complete.
    case ReplayCompleteMessage => {
        enableAgentSelection(true)
        enableCompetitionButtons(true,true,false)      
    }
  }
  
  /**
   * Agent selection has changed.
   */
  private def agentSelectionChanged = {
    
    // Process selection and update text field.
    def processSelected[T](selected : Option[T],textField : TextField) = selected match {
      case None => {
        textField.text = ""
        textField.background = Color.LIGHT_GRAY
        false
      }
      case Some(item) => {
        textField.text = item.toString
        textField.background = Color.WHITE
        true
      }      
    }
    
    // Update selected generations
    processSelected[Int](evaderGeneration,selectedEvaderGenerationField)
    processSelected[Int](pursuerGeneration,selectedPursuerGenerationField)
    
    processSelected[String](evaderFitness,selectedEvaderFitnessField)
    processSelected[String](pursuerFitness,selectedPursuerFitnessField)
    
    // Update selected Agents
    val evaderSelected = processSelected[Agent](evader,selectedEvaderField)
    val pursuerSelected = processSelected[Agent](pursuer,selectedPursuerField)
   
    enableCompetitionButtons(evaderSelected && pursuerSelected,false,false)
  }
  
  /**
   * Enable/disable agent selection.
   */
  private def enableAgentSelection(isEnabled : Boolean) = {
    generationListViews.map(_.enabled = isEnabled)
    agentListViews.map(_.enabled = isEnabled)
    this.agentTextFields.map(_.enabled = isEnabled)
  }
  
  /**
   * Set state of competition panel buttons.
   */
  private def enableCompetitionButtons(runEnabled : Boolean,replayEnabled : Boolean,stopEnabled : Boolean) = {
    runCompetitionButton.enabled = runEnabled
    replayCompetitionButton.enabled = replayEnabled
    stopCompetitionButton.enabled = stopEnabled
  }
  
  /**
   * Reset panel.
   * 
   * Clean up anything from previous run.
   */
  def reset = {
    
    // Clear agents
    evaders.clear
    pursuers.clear
    
    // Clear List Views
    evaderGenerationListView.listData = List()
    pursuerGenerationListView.listData = List()
    evadersListView.listData = List()
    pursuersListView.listData = List()
    
    // Clear selections
    evader = None
    pursuer = None
    evaderFitness = None
    pursuerFitness = None
    evaderGeneration = None
    pursuerGeneration = None
    
    agentSelectionChanged
    
    // Update button states.
    enableAgentSelection(true)
    enableCompetitionButtons(false,false,false)   
  }
}
