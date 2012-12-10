package com.matthewtyler.pe.tournament

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.swing.Publisher

import akka.actor.{Actor,ActorRef,Props}

import com.matthewtyler.pe.agent.Agent
import com.matthewtyler.pe.competition.{PEAgentResult, PECompetition, PECompetitionResult}
import com.matthewtyler.pe.constraints.{AgentConstraints,TournamentConstraints}
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.random.RandomFactory
import com.matthewtyler.pe.tournament.messages._

/**
 * Class representing a Pursuer Evader tournament.
 * 
 * Immutable
 */
class PETournament(private val tournamentSession : Int,
                   private val tournamentConstraints : TournamentConstraints) extends Actor with Publisher with Logging {
  
  /**
   * preStart processing.
   */
  override def preStart = {
    info("Starting PETournament - Session %s".format(tournamentSession))
  }
  
  /**
   * postStop processing.
   */
  override def postStop = {
    info("PETournament - Session %s has stopped.".format(tournamentSession))
  }
      
  /**
   * Handle messages when tournament is in started state.
   */
  private def tournamentStarted(tournamentRunner : ActorRef,
                                generation : Int,
                                evaders : Map[String,Agent],
                                pursuers : Map[String,Agent]) : Receive = {    
                  
    // Tournament result message.
    case PETournamentResultMessage(result,`tournamentSession`) => {
        
      debug("PETournament received PETournamentResultMessage. Current Generation %s. Session %s".format(generation,tournamentSession))
                  
      val (newEvaders,newPursuers) = update(result,evaders,pursuers,generation)
          
      // Send result to manager
      context.parent ! PETournamentResultMessage(result,tournamentSession)
            
      // If tournament complete manager.
      if(generation == tournamentConstraints.generations) {
            
        info("Tournament Complete. Killing PETournament, session {}, generation {}.",tournamentSession,generation)
                                   
        // Notify manager that tournament is complete
        context.parent ! PETournamentCompleteMessage(tournamentSession)
            
        // Stop self and children
        context.stop(self)
      }
      else {
        
        debug("Starting generation {}",generation + 1)
        
        // Tournament not complete.
        // Run next generation of competitions.
        tournamentRunner ! PETournamentMessage(generateCompetitions(newEvaders,newPursuers),tournamentSession)
           
        // Remain in started state and run next generation
        context.become(tournamentStarted(tournamentRunner,generation + 1,newEvaders,newPursuers))
      }
    }         
  }
     
  /**
   * Receive method.
   * 
   * Process incoming PETournament messages 
   */
  def receive = {
    
    // StartMessage
    case startMessage : PETournamentStartMessage => {
        
      debug("Starting new Tournament")
         
      val peTournamentRunner = context.actorOf(Props(PETournamentRunner(startMessage.session,startMessage.tournamentConstraints.competitionActors)), name = "PETournamentRunner_%s".format(this.tournamentSession))        
      
      // Start tournament runner
      peTournamentRunner ! startMessage
        
      // Initialise populations
      val (evaders,pursuers) = initialise(startMessage.tournamentConstraints.population,
                                          startMessage.evaderConstraints,
                                          startMessage.pursuerConstraints)
        
      // Start first tournament
      peTournamentRunner ! PETournamentMessage(generateCompetitions(evaders,pursuers),tournamentSession)
          
      // Enter started state.
      context.become(tournamentStarted(peTournamentRunner,1,evaders,pursuers))        
    }
  }
  
  /**
   * Generate round robin competitions between all pursuers and evaders
   */
  private def generateCompetitions(evaders : Map[String,Agent],pursuers : Map[String,Agent]) = {
    
    val competitions = for(evader <- evaders.values; pursuer <- pursuers.values) yield PECompetition(evader,pursuer,tournamentConstraints)
   
    competitions.toList
  }
  
  /**
   * Initialise populations
   */
  private def initialise(populationSize : Int,
                         evaderConstraints : AgentConstraints,
                         pursuerConstraints : AgentConstraints,
                         evaders : Map[String,Agent] = Map[String,Agent](),
                         pursuers : Map[String,Agent] = Map[String,Agent]()) : (Map[String,Agent],Map[String,Agent]) = {
    
    
    if(populationSize == 0) {
      debug("Populations initialized.")
      (evaders,pursuers)
    }
    else {
      val evader = Agent.newRandomEvader(evaderConstraints,tournamentConstraints)
      val pursuer = Agent.newRandomPursuer(pursuerConstraints,tournamentConstraints)
      
      initialise(populationSize-1,
                 evaderConstraints,
                 pursuerConstraints,
                 evaders + (evader.toString -> evader),
                 pursuers + (pursuer.toString -> pursuer))      
    }
  }

  /**
   * Generate the next generation of Agents
   */
  private def nextGeneration(agents : Map[String,Agent],scores : Map[String,PEAgentResult]): Map[String,Agent] = {
     
    val sortedScores = scores.values.toList.sortWith((a,b) => a.averageScore < b.averageScore)
    
    // Cull worst performing agents and replace with new offspring.
    @tailrec def process(sortedScores : List[PEAgentResult],agents : Map[String,Agent],remove : Int) : Map[String,Agent] = sortedScores match {
      
      case List() => throw new RuntimeException("Unexpected Nil list received.")
      
      // Cull worst performing members
      case h :: t if(remove > 0) => {
        debug("Culling {}", h)        
        process(t,agents -(h.agent),remove - 1)
      }
      
      // Worst performing Agents have been culled.
      // Generate replacement offspring
      case h :: t => {
        
        // Generate the sum of average scores of all remaining agents.
        // This will be used when selecting offspring.
        val totalScore = sortedScores.map(_.averageScore).sum
        
        // Generate offspring
        @tailrec def addOffspring(agents : Map[String,Agent],remaining : Int) : Map[String,Agent] = {
          
          // Disaster strikes!
          if(remaining < 0) throw new RuntimeException("Error generating offspring. Remaining %s".format(remaining))
          
          // All offspring created - return new population.
          else if(remaining == 0) agents
          
          // Offspring remaining to be created.
          else {
            
            // Select two parents with probability proportional to fitness.
            // Mate them and add offspring to population.
            val parent1 = selectRandom(sortedScores,agents,totalScore)
            val parent2 = selectRandom(sortedScores,agents,totalScore,scores(parent1.toString))
            
            val offspring = parent1 mate parent2
            
            addOffspring(agents +(offspring.toString -> offspring) ,remaining-1)
          }
        }
        
        addOffspring(agents,tournamentConstraints.agentsToCull)
      }      
    }
          
    process(sortedScores,agents,tournamentConstraints.agentsToCull)    
  }
  
  
  /**
   * Calculate evader/pursuer scores
   */
  @tailrec private def getScores(competitionResults : List[PECompetitionResult],
                        evaderScores : Map[String,PEAgentResult] = Map[String,PEAgentResult](),
                        pursuerScores : Map[String,PEAgentResult] = Map[String,PEAgentResult]()) : (Map[String,PEAgentResult],Map[String,PEAgentResult]) = competitionResults match {
    
    // All results processed.
    case Nil => (evaderScores,pursuerScores)
   
    // Keep on truckin!
    case h :: t => {
      
      val evaderResult = h.evaderResult
      val pursuerResult = h.pursuerResult
      
      getScores(t,
                if(evaderScores.contains(evaderResult.agent)) evaderScores +(evaderResult.agent -> (evaderScores(evaderResult.agent) + evaderResult)) 
                else evaderScores + (evaderResult.agent -> evaderResult),
                if(evaderScores.contains(pursuerResult.agent))pursuerScores +(pursuerResult.agent -> (pursuerScores(pursuerResult.agent) + pursuerResult))
                else pursuerScores + (pursuerResult.agent -> pursuerResult))      
    }   
  }
  
  /**
   * Process list of PECompetitionResult.
   * 1. Remove poorest performing Agents from population.
   * 2. Replace with offspring.
   */
  private def update(competitionResults : List[List[PECompetitionResult]],
                     evaders            : Map[String,Agent],
                     pursuers           : Map[String,Agent],
                     generation         : Int) = {
    
    val (evaderScores,pursuerScores) = getScores(competitionResults.flatten,Map[String,PEAgentResult](),Map[String,PEAgentResult]())
         
    debug("Generation {} average evader fitness {}, Escape events {}.",generation,evaderScores.values.map(x => x.averageScore).sum / evaderScores.size, evaderScores.values.map(x => x.bonusEvents).sum)  
    debug("Generation {} average pursuer fitness {}, Capture events {}. ",generation,pursuerScores.values.map(x => x.averageScore).sum / pursuerScores.size, pursuerScores.values.map(x => x.bonusEvents).sum)
      
    debug("Generating next population.")
    
    // Generate next generations.
    (nextGeneration(evaders,evaderScores),nextGeneration(pursuers,pursuerScores))
  }
    
  /**
   * Select an Agent with probability of selection proportional to PEAgentResult.averageScore.
   * Omit ignoreAgent from selection if == ""
   */
  private def selectRandom(scores : List[PEAgentResult],agents : Map[String,Agent],totalScore : Double,ignoreAgent : PEAgentResult = null): Agent  = {
     
    val scoreSum = if(ignoreAgent == null) totalScore else totalScore - ignoreAgent.averageScore
    val randomScore = RandomFactory.nextDoubleInRange(scoreSum)
 
    debug("REQUIRED SCORE {}, SUM {}, IGNORE {}.",randomScore,scoreSum,ignoreAgent)
    debug("SCORES {}",scores)
    
    // Select a random agent skipping ignoreAgent if it is != ""
    @tailrec def select(scores : List[PEAgentResult],cumulativeScore : Double): Agent = scores match {      
      // Disaster strikes
      case List() => throw new RuntimeException("Unexpected Nil list.");
      // Head Agent == Ignore Agent. Skip.
      case h :: t if h.toString == ignoreAgent => select(t,cumulativeScore)
      // randomScore lies within this score bucket. Return Agent
      case h :: t if cumulativeScore + h.averageScore >= randomScore => agents(h.agent)
      // Continue search.
      case h :: t => select(t,cumulativeScore + h.averageScore)        
    }
    
    val selected = select(scores,0)
    
    debug("SELECTED {}",selected)
    
    selected
  }  
}


