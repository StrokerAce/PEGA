package com.matthewtyler.pe.gui

import java.awt.Color

import javax.swing.border.LineBorder
import javax.swing.border.TitledBorder

import scala.collection.immutable.HashMap

import scala.swing._
import scala.swing.Dialog._
import scala.swing.event.{EditDone,SelectionChanged}

import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.logging.Logging

/**
 * Constraints Panel
 */
object ConstraintsPanel extends GridPanel(1,3) with Logging {
  
  import scala.math._
  
  // Tournament constraints dialogue
  class TournamentConstraintsDialogue(defaultGenerations   : Int,
                                      defaultPopulation    : Int,
                                      defaultHeats         : Int,
                                      defaultStartDistance : Double,
                                      defaultCaptureDistance : Double,
                                      defaultMutationRate : Double,
                                      defaultAgentsToCull : Int,
                                      defaultCompetitionActors : Int,
                                      defaultRandomSeed : Long) extends GridPanel(9,2) {
    
    border = new TitledBorder(new LineBorder(Color.BLACK),"Tournament Constraints")
    
    hGap =5
    vGap = 5
    
     val generationsTextField = new TextField {
      text = defaultGenerations.toString
      tooltip = "Number of generations for which Genetic Algorith will run"
      editable = true
      horizontalAlignment = Alignment.Right
            
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Number of generations must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }
    
    val populationTextField = new TextField {
      text = defaultPopulation.toString
      tooltip = "Evader/pursuer Agent population size"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Population must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }
    
    val heatsTextField = new TextField {
      text = defaultHeats.toString
      tooltip = "Heats per competition"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Heats must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }
    
    val startDistanceTextField = new TextField {
      text = defaultStartDistance.toString
      tooltip = "Distance between agents at t=0"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Start Distance must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    val captureDistanceTextField = new TextField {
      text = defaultCaptureDistance.toString
      tooltip = "Distance between Agents for a capture event to occur."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Capture Distance must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }   
 
    
    val mutationRateTextField = new TextField {
      text = defaultMutationRate.toString
      tooltip = "Mutation rate."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x >= 0.0 && x <= 1.0) Right(x) else Left("Mutation Rate must be >= 0 and <= 1.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    } 
  
    val agentsToCullTextField = new TextField {
      text = defaultAgentsToCull.toString
      tooltip = "Number of worst performing Agents to be replaced each generation."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0 && x < GuiHelper.getOrThrow[Int](populationTextField.getConverted)) Right(x) 
                                                           else Left("Agents To Cull must be > 0 and < population")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }  
    
    val competitionActorsTextField = new TextField {
      text = defaultCompetitionActors.toString
      tooltip = "Number of Actors Competitions will be partitioned between."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Competition Actors must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }       

    val randomSeedTextField = new TextField {
      text = defaultRandomSeed.toString
      tooltip = "Seed for Random Number Generator."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Long): Either[String,Long] = Right(x)
      
      def getConverted = GuiHelper.convertAndValidate[Long](GuiHelper.parseLong(text))(validator)
    }  
    
    // All TextFields
    val textFields = List(generationsTextField,
                          populationTextField,
                          heatsTextField,
                          startDistanceTextField,
                          captureDistanceTextField,
                          mutationRateTextField,
                          agentsToCullTextField,
                          competitionActorsTextField,
                          randomSeedTextField)
                          
   // Build Constraints Dialogue    
    contents += new Label{
      text = "Generations"
      xAlignment = Alignment.Left
    }
    
    contents += generationsTextField
    
    contents += new Label{
      text = "Population"
      xAlignment = Alignment.Left
    }
    
    contents += populationTextField
    
    contents += new Label{
      text = "Heats"
      xAlignment = Alignment.Left
    }
    
    contents += heatsTextField  
    
    contents += new Label{
      text = "Start Distance"
      xAlignment = Alignment.Left
    }
    
    contents += startDistanceTextField

    contents += new Label{
      text = "Capture Distance"
      xAlignment = Alignment.Left
    }
    
    contents += captureDistanceTextField
    
    contents += new Label{
      text = "Mutation Rate"
      xAlignment = Alignment.Left
    }
    
    contents += mutationRateTextField
 
    contents += new Label{
      text = "Agents To Cull"
      xAlignment = Alignment.Left
    }
    
    contents += agentsToCullTextField
    
    contents += new Label{
      text = "Competition Actors"
      xAlignment = Alignment.Left
    }
    
    contents += competitionActorsTextField
    
    contents += new Label{
      text = "Random Seed"
      xAlignment = Alignment.Left
    }
    
    contents += randomSeedTextField
    
   /*
     * Register events
     */
    textFields.map(listenTo(_))
    
    /*
     * Process events.
     */
    reactions += {
      
      // Validate Generations
      case EditDone(`generationsTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         generationsTextField,
                                         defaultGenerations.toString,
                                         generationsTextField.getConverted,
                                         "Generations")
                                         
      // Validate Population                                                          
      case EditDone(`populationTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         populationTextField,
                                         defaultPopulation.toString,
                                         populationTextField.getConverted,
                                         "Population")
                         
      // Validate Heats                                                               
      case EditDone(`heatsTextField`) =>
        GuiHelper.processTextFieldUpdate(this,
                                         heatsTextField,
                                         defaultHeats.toString,
                                         heatsTextField.getConverted,
                                         "Heats")                                        
 
      // Validate Start Distance                                                               
      case EditDone(`startDistanceTextField`) =>
        GuiHelper.processTextFieldUpdate(this,
                                         startDistanceTextField,
                                         defaultStartDistance.toString,
                                         startDistanceTextField.getConverted,
                                         "Start Distance")
 
                                                        
      // Validate Capture Distance                                                   
      case EditDone(`captureDistanceTextField`) =>
        GuiHelper.processTextFieldUpdate(this,
                                         captureDistanceTextField,
                                         defaultCaptureDistance.toString,
                                         captureDistanceTextField.getConverted,
                                         "Capture Distance")
                                            
      // Validate Mutation Rate                                                   
      case EditDone(`mutationRateTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         mutationRateTextField,
                                         defaultMutationRate.toString,
                                         mutationRateTextField.getConverted,
                                         "Mutation Rate")
                                         
      // Validate Agents To Cull                                                  
      case EditDone(`agentsToCullTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         agentsToCullTextField,
                                         defaultAgentsToCull.toString,
                                         agentsToCullTextField.getConverted,
                                         "Agents To Cull")                                         
                                         
      // Validate Competition Actors                                                   
      case EditDone(`competitionActorsTextField`) =>
        GuiHelper.processTextFieldUpdate(this,
                                         competitionActorsTextField,
                                         defaultCompetitionActors.toString,
                                         competitionActorsTextField.getConverted,
                                         "Competition Actors")

      // Validate Random Seed                                                   
      case EditDone(`randomSeedTextField`) =>
        GuiHelper.processTextFieldUpdate(this,
                                         randomSeedTextField,
                                         defaultRandomSeed.toString,
                                         randomSeedTextField.getConverted,
                                         "Random Seed")                                         
    }
    
    /**
     * Get Tournament Constraints.
     */
    def getConstraints = 
      TournamentConstraints(GuiHelper.getOrThrow[Int](generationsTextField.getConverted),
                            GuiHelper.getOrThrow[Int](populationTextField.getConverted),
                            GuiHelper.getOrThrow[Int](heatsTextField.getConverted),
                            GuiHelper.getOrThrow[Double](startDistanceTextField.getConverted),
                            GuiHelper.getOrThrow[Double](captureDistanceTextField.getConverted),
                            GuiHelper.getOrThrow[Double](mutationRateTextField.getConverted),
                            GuiHelper.getOrThrow[Int](agentsToCullTextField.getConverted),
                            GuiHelper.getOrThrow[Int](competitionActorsTextField.getConverted),
                            GuiHelper.getOrThrow[Long](randomSeedTextField.getConverted))
    
    /**
     * Enable/Disable text fields
     */
    def setEnabled(isEnabled : Boolean) = textFields.map(_.enabled = isEnabled)
  }
  
  // Agent constraints dialogue
  class AgentConstraintsDialogue(borderTitle : String,
                                 defaultAngularBuckets : Int,
                                 defaultRadialBuckets : Int,
                                 defaultRadialBucketMaxSize : Int,
                                 defaultMass : Double,
                                 defaultMaxForce : Double,
                                 defaultMaxSpeed : Double,
                                 defaultStartingEnergy : Double,
                                 defaultMaxAngularVelocity : Double,
                                 defaultMaxAngularVelocityAtMaxSpeed : Double,
                                 defaultBonusPenalty : Double,
                                 defaultWinBonus : Double,
                                 defaultPointsPerPositionImprovement : Double,
                                 improvementPredicate : Function1[Double,Boolean]) extends GridPanel(13,2) {
    
    border = new TitledBorder(new LineBorder(Color.BLACK),borderTitle)
    
    hGap =5
    vGap = 5
    
    val angularBucketsTextField = new TextField {
      text = defaultAngularBuckets.toString
      tooltip = "Number of angular buckets. Size = 2Pi/Number"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Number of Angular Buckets must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }
    
    val radialBucketsTextField = new TextField {
      text = defaultRadialBuckets.toString
      tooltip = "Number of Radial Buckets"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Number of Radial Buckets must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }    
 
    val radialBucketMaxSizeTextField = new TextField {
      text = defaultRadialBucketMaxSize.toString
      tooltip = "Radial Bucket Max Size"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Int): Either[String,Int] = if(x > 0) Right(x) else Left("Radial Bucket Max Size must be > 0")
      
      def getConverted = GuiHelper.convertAndValidate[Int](GuiHelper.parseInt(text))(validator)
    }
 
    val massTextField = new TextField {
      text = defaultMass.toString
      tooltip = "Agent mass"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Agent Mass must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }    
    
    val maxForceTextField = new TextField {
      text = defaultMaxForce.toString
      tooltip = "Max force Agent can apply to change speed"
      editable = true
      horizontalAlignment = Alignment.Right

      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Max Force must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    val maxSpeedTextField = new TextField {
      text = defaultMaxSpeed.toString
      tooltip = "Agent Max Speed"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Max Speed must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    val startingEnergyTextField = new TextField {
      text = defaultStartingEnergy.toString
      tooltip = "Agent default energy"
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0) Right(x) else Left("Starting Energy must be > 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    val maxAngularVelocityTextField = new TextField {
      text = defaultMaxAngularVelocity.toString
      tooltip = "Agent Max Angular Velocity"
      editable = true
      horizontalAlignment = Alignment.Right

      private def validator(x : Double): Either[String,Double] = if(x > 0.0 && x <= Pi) Right(x) else Left("Agent Max Angular Velocity must be > 0.0 and <= PI")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    val maxAngularVelocityAtMaxSpeedTextField = new TextField {
      text = defaultMaxAngularVelocityAtMaxSpeed.toString
      tooltip = "Ratio of Maximum Angular Velocity at Max Speed."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x > 0.0 && x <= 1.0) Right(x) else Left("Max Angular Velocity At Max Speed must be > 0.0 and <= 1.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }   
 
    val winBonusTextField = new TextField {
      text = defaultWinBonus.toString
      tooltip = "Agent Win Bonus."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x >= 0.0) Right(x) else Left("Win Bonus must be >= 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }

    val bonusPenaltyTextField = new TextField {
      text = defaultBonusPenalty.toString
      tooltip = "Bonus points dropped per non winning move."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x >= 0.0) Right(x) else Left("Points per position improvement must be >= 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }    
    
    val pointsPerPositionImprovementTextField = new TextField {
      text = defaultPointsPerPositionImprovement.toString
      tooltip = "Points per position improvement."
      editable = true
      horizontalAlignment = Alignment.Right
      
      private def validator(x : Double): Either[String,Double] = if(x >= 0.0) Right(x) else Left("Points per position improvement must be >= 0.0")
      
      def getConverted = GuiHelper.convertAndValidate[Double](GuiHelper.parseDouble(text))(validator)
    }
    
    
    val booleanSelection = List(true,false)
    
    val addEnergyToWinningScoreComboBox = new ComboBox(booleanSelection) {
      selection.item = false
      enabled = true
    }
    
    // All TextFields
    val textFields = List(angularBucketsTextField,
                          radialBucketsTextField,
                          radialBucketMaxSizeTextField,
                          massTextField,
                          maxForceTextField,
                          maxSpeedTextField,
                          startingEnergyTextField,
                          maxAngularVelocityTextField,
                          maxAngularVelocityAtMaxSpeedTextField,
                          winBonusTextField,
                          bonusPenaltyTextField,
                          pointsPerPositionImprovementTextField)
    
   
    // Build Constraints Dialogue    
    contents += new Label{
      text = "Angular Buckets"
      xAlignment = Alignment.Left
    }
    
    contents += angularBucketsTextField
    
    contents += new Label{
      text = "Radial Buckets"
      xAlignment = Alignment.Left
    }
    
    contents += radialBucketsTextField
    
    contents += new Label{
      text = "Radial Bucket Max Size"
      xAlignment = Alignment.Left
    }
    
    contents += radialBucketMaxSizeTextField  
    
    contents += new Label{
      text = "Mass"
      xAlignment = Alignment.Left
    }
    
    contents += massTextField

    contents += new Label{
      text = "Max Force"
      xAlignment = Alignment.Left
    }
    
    contents += maxForceTextField
    
    contents += new Label{
      text = "Max Speed"
      xAlignment = Alignment.Left
    }
    
    contents += maxSpeedTextField
    
    contents += new Label{
      text = "Starting Energy"
      xAlignment = Alignment.Left
    }
    
    contents += startingEnergyTextField
    
    contents += new Label{
      text = "Max Angular Velocity ( x PI )"
      xAlignment = Alignment.Left
    }
    
    contents += maxAngularVelocityTextField
    
   contents += new Label{
      text = "Win Bonus"
      xAlignment = Alignment.Left
    }
    
    contents += winBonusTextField
   
    contents += new Label{
      text = "Max Angular Velocity at Max Speed"
      xAlignment = Alignment.Left
    }
    
    contents += maxAngularVelocityAtMaxSpeedTextField    
 
    contents += new Label{
      text = "Bonus penalty"
      xAlignment = Alignment.Left
    }
    
    contents += bonusPenaltyTextField    
    
    contents += new Label{
      text = "Points per position improvement"
      xAlignment = Alignment.Left
    }
    
    contents += pointsPerPositionImprovementTextField
    
    contents += new Label{
      text = "Add remaining energy to winning score"
      xAlignment = Alignment.Left
    }
    
    contents += addEnergyToWinningScoreComboBox
    
    /*
     * Register events.
     */
    textFields.map(listenTo(_))
    listenTo(addEnergyToWinningScoreComboBox.selection)
    
    /*
     * Process events.
     */
    reactions += {
      
      // Validate Angular Buckets
      case EditDone(`angularBucketsTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         angularBucketsTextField,
                                         defaultAngularBuckets.toString,
                                         angularBucketsTextField.getConverted,
                                         "AngularBuckets")
      
      // Validate Radial Buckets                                                          
      case EditDone(`radialBucketsTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         radialBucketsTextField,
                                         defaultRadialBuckets.toString,
                                         radialBucketsTextField.getConverted,
                                         "Radial Buckets")
                         
      // Validate Radial Bucket max size                                                               
      case EditDone(`radialBucketMaxSizeTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         radialBucketMaxSizeTextField,
                                         defaultRadialBucketMaxSize.toString,
                                         radialBucketMaxSizeTextField.getConverted,
                                         "Radial Bucket Max Size")
                                        
      // Validate mass                                                               
      case EditDone(`massTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         massTextField,
                                         defaultMass.toString,
                                         massTextField.getConverted,
                                         "Mass")
                                                        
      // Validate Max Force                                                   
      case EditDone(`maxForceTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         maxForceTextField,
                                         defaultMaxForce.toString,
                                         maxForceTextField.getConverted,
                                         "Max Force")
                                                             
      // Validate Max Speed                                                       
      case EditDone(`maxSpeedTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         maxSpeedTextField,
                                         defaultMaxSpeed.toString,
                                         maxSpeedTextField.getConverted,
                                         "Max Speed")

      // Validate starting energy                                                   
      case EditDone(`startingEnergyTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         startingEnergyTextField,
                                         defaultStartingEnergy.toString,
                                         startingEnergyTextField.getConverted,
                                         "Starting Energy")  
      // Validate Max Angular Velocity                                                   
      case EditDone(`maxAngularVelocityTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         maxAngularVelocityTextField,
                                         defaultMaxAngularVelocity.toString,
                                         maxAngularVelocityTextField.getConverted,
                                         "Max Angular Velocity")
                                                         
      // Validate Max Angular Velocity at Max Speed                                                         
      case EditDone(`maxAngularVelocityAtMaxSpeedTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         maxAngularVelocityAtMaxSpeedTextField,
                                         defaultMaxAngularVelocityAtMaxSpeed.toString,
                                         maxAngularVelocityAtMaxSpeedTextField.getConverted,
                                         "Max Angular Velocity At max Speed")
                                         
      // Validate Win Bonus                                                         
      case EditDone(`winBonusTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         winBonusTextField,
                                         defaultWinBonus.toString,
                                         winBonusTextField.getConverted,
                                         "Win Bonus")

      // Validate Bonus Penalty                                                        
      case EditDone(`bonusPenaltyTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         bonusPenaltyTextField,
                                         defaultBonusPenalty.toString,
                                         bonusPenaltyTextField.getConverted,
                                         "Bonus penalty")                                          
                                         
      // Validate Points per position improvement                                                        
      case EditDone(`pointsPerPositionImprovementTextField`) => 
        GuiHelper.processTextFieldUpdate(this,
                                         pointsPerPositionImprovementTextField,
                                         defaultPointsPerPositionImprovement.toString,
                                         pointsPerPositionImprovementTextField.getConverted,
                                         "Points per position improvement") 
                                         
      case SelectionChanged(`addEnergyToWinningScoreComboBox`) => {
        info("{} add energy to winning score changed to {}.",borderTitle,addEnergyToWinningScoreComboBox.selection.item)
      }
    }
    
    /**
     * Get set of Constraints.
     */
    def getConstraints(agentType : String) : AgentConstraints = {
      AgentConstraints(agentType,
                       GuiHelper.getOrThrow[Int](angularBucketsTextField.getConverted),
                       GuiHelper.getOrThrow[Int](radialBucketsTextField.getConverted),
                       GuiHelper.getOrThrow[Int](radialBucketMaxSizeTextField.getConverted),
                       GuiHelper.getOrThrow[Double](maxForceTextField.getConverted), 
                       GuiHelper.getOrThrow[Double](maxSpeedTextField.getConverted), 
                       GuiHelper.getOrThrow[Double](maxAngularVelocityTextField.getConverted),
                       GuiHelper.getOrThrow[Double](maxAngularVelocityAtMaxSpeedTextField.getConverted),
                       GuiHelper.getOrThrow[Double](massTextField.getConverted),
                       GuiHelper.getOrThrow[Double](startingEnergyTextField.getConverted),
                       GuiHelper.getOrThrow[Double](winBonusTextField.getConverted),
                       GuiHelper.getOrThrow[Double](pointsPerPositionImprovementTextField.getConverted),
                       GuiHelper.getOrThrow[Double](bonusPenaltyTextField.getConverted),
                       addEnergyToWinningScoreComboBox.selection.item,
                       improvementPredicate)
    }
    
    /**
     * Enable/Disable components.
     */
    def setEnabled(isEnabled : Boolean) = {
      textFields.map(_.enabled = isEnabled)
      addEnergyToWinningScoreComboBox.enabled = isEnabled
    }
  }
  
  hGap = 5
  
  // Agent constraints dialogues
  lazy val evaderDialogue = new AgentConstraintsDialogue("Evader Constraints",18,12,50,5.0,50.0,50.0,7000.0,0.6,0.75,0.0,0.0,20.0,(deltaDistance : Double) => deltaDistance > 0.0)
  lazy val pursuerDialogue = new AgentConstraintsDialogue("Pursuer Constraints",18,12,50,5.0,90.0,90.0,10000.0,0.6,0.75,200.0,10000.0,10.0,(deltaDistance : Double) => deltaDistance < 0.0)
  lazy val tournamentDialogue = new TournamentConstraintsDialogue(1000,100,1,100.0,10.0,0.1,15,Runtime.getRuntime.availableProcessors-1,12345678L)

  lazy val agentDialogues = List(evaderDialogue,pursuerDialogue)
  
  // Build Constraints Panel
  contents ++= agentDialogues
  contents += tournamentDialogue
  
  /**
   * Get Evader Constraints
   */
  def evaderConstraints = evaderDialogue.getConstraints("Evader")
  
  /**
   * Get Pursuer Constraints
   */
  def pursuerConstraints = pursuerDialogue.getConstraints("Pursuer")
  
  /**
   * Get Tournament Constraints.
   */
  def tournamentConstraints = tournamentDialogue.getConstraints
  
  /**
   * Enable/Disable dialogues
   */
  def setEnabled(isEnabled : Boolean) = {
    agentDialogues.map(_.setEnabled(isEnabled))
    tournamentDialogue.setEnabled(isEnabled)
  }
}