package com.matthewtyler.pe.gui

import java.awt.{Color, Dimension, Graphics, Image}
import javax.swing.JPanel

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props}
import com.matthewtyler.pe.competition.PECompetitionResult
import com.matthewtyler.pe.gui.messages.ReplayCompleteMessage
import com.matthewtyler.pe.logging.Logging
import com.matthewtyler.pe.state.pestate.PEState
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.{Panel, Publisher, Swing}

/**
  * Agent Competition Canvas.
  */
object AgentCompetitionCanvas extends Panel with Logging with Publisher {

  import scala.math._

  /**
    * Size of our canvas
    */
  private val maxWidth = 1600
  private val maxHeight = 800
  private val usableArea = 0.97

  // centre point
  private val xOffset = maxWidth.toDouble / 2.0
  private val yOffset = maxHeight.toDouble / 2.0

  private val expansionRatio = 20
  private val millisBetweenUpdates = 10

  // Font size for trace data
  private val fontSize = 10

  // Spacing between rows.
  private val spacing = fontSize + 4

  // Width of trace data.
  private val traceDataWidth = 14

  private val evaderXOffset = spacing
  private val pursuerXOffset = maxWidth - spacing - (traceDataWidth * fontSize)

  private val agentTraceFont = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, spacing)
  private val pursuerColour = Color.RED
  private val evaderColour = Color.GREEN


  /**
    * Agent vapour trail.
    */
  case class AgentTrace(traceColor: Color, states: List[PEState], dataXOffset: Int) {

    // Size of our trace dots...
    private val maxDotSize = 20

    // Number of states to skip between dots.
    private val skip = 10

    /**
      * Add new state
      */
    def addState(newState: PEState) = states match {
      case Nil => AgentTrace(traceColor, List(newState), dataXOffset)
      case _ => AgentTrace(traceColor, newState :: states, dataXOffset)
    }

    /**
      * Draw vapour trail...........
      */
    def paint(g: Graphics) = {

      def doIt(statesToPaint: List[PEState], size: Int, count: Int): Unit = statesToPaint match {
        case h :: t if size > 0 =>

          val paintMe = (count % skip) == 0

          if (paintMe) {
            val x = ((h.position.i * xyRatio) + xOffset).toInt
            val y = ((h.position.j * xyRatio) + yOffset).toInt

            if (size % 2 == 0) g.setColor(Color.WHITE) else g.setColor(traceColor)
            g.fillOval(x, y, size, size)
          }

          doIt(t, size - (if (paintMe) 1 else 0), count + 1)
        case _ => // Do nothing
      }

      doIt(states, maxDotSize, 0)

      // Update scores and (if applicable) winner.
      val (score, energy, speed, heading, winString) = states match {
        case h :: _ => (h.score, h.energy, h.velocity.magnitude, h.velocity.theta, if (h.winningState) "*** WINNER! ***" else "")
        case _ => (0.0, 0.0, 0.0, 0.0, "")
      }

      // Add stats.
      g.setColor(traceColor)
      g.setFont(agentTraceFont)
      g.drawString("Score   : %08.2f".format(score), dataXOffset, spacing)
      g.drawString("Speed   : %08.2f".format(speed), dataXOffset, (2 * spacing))
      g.drawString("Heading : %08.2f".format(heading), dataXOffset, (3 * spacing))
      g.drawString("Energy  : %08.2f".format(energy), dataXOffset, (4 * spacing))
      g.drawString(winString, dataXOffset, (5 * spacing))
    }
  }

  /**
    * Runs competition.
    */
  class CompetitionActor(private val result: PECompetitionResult) extends Actor {

    import java.util.concurrent.TimeUnit.MILLISECONDS

    import scala.concurrent.duration.FiniteDuration

    /**
      * preStart processing.
      */
    override def preStart = {
      info("Starting CompetitionActor")

      // Add new Trace objects.
      AgentCompetitionCanvas.evaderTrace = AgentTrace(evaderColour, List(), evaderXOffset)
      AgentCompetitionCanvas.pursuerTrace = AgentTrace(pursuerColour, List(), pursuerXOffset)
    }

    /**
      * postStop processing.
      */
    override def postStop = {
      info("CompetitionActor has stopped")
    }

    /**
      * Iterates through list of competition states and updates canvas every x milliseconds.
      */
    def update(evaderStates: List[PEState], pursuerStates: List[PEState], cancellable: Cancellable): Receive = {

      // Stop competition replay
      case 'DIE =>
        info("Stopping CompetitionActor")
        cancellable.cancel
        context.stop(self)
      // Time for next update.
      case 'Tick =>
        evaderStates match {
          case Nil =>
            debug("CompetitionActor replay complete. Goodbye!")
            Swing.onEDT(AgentCompetitionCanvas.publish(ReplayCompleteMessage))
            cancellable.cancel
            context.stop(self)
          case _ =>
            Swing.onEDT(Canvas.update(evaderStates.head, pursuerStates.head))
            context.become(update(evaderStates.tail, pursuerStates.tail, cancellable))
        }
    }

    def receive = {

      case 'START =>
        val expandedResult = result.expand(expansionRatio)
        context.system.scheduler.scheduleOnce(FiniteDuration(millisBetweenUpdates, MILLISECONDS)) {
          self ! 'Tick
        }

        val cancellable = context.system.scheduler.schedule(FiniteDuration(0, MILLISECONDS),
          FiniteDuration(millisBetweenUpdates, MILLISECONDS)) {
          self ! 'Tick
        }

        context.become(update(expandedResult.evaderStates, expandedResult.pursuerStates, cancellable))
    }
  }

  // XY ratio for competition.
  private var xyRatio = 1.0

  /**
    * Configure akka scheduler to have a higher resolution tick.
    */
  val customConf = ConfigFactory.parseString(
    """
    akka.scheduler {
      tick-duration = 10ms
    }""")

  private val agentCompetitionActorSystem = ActorSystem("agentCompetitionActorSystem", ConfigFactory.load(customConf))

  private var actorRef: ActorRef = null

  private var evaderTrace = AgentTrace(evaderColour, List(), evaderXOffset)
  private var pursuerTrace = AgentTrace(pursuerColour, List(), pursuerXOffset)

  /**
    * Start competition.
    */
  def start(competitionResult: PECompetitionResult) = {

    val (maxX, maxY) = competitionResult.maxXY

    xyRatio = min((xOffset / maxX), (yOffset / maxY)) * usableArea

    actorRef = agentCompetitionActorSystem.actorOf(Props(new CompetitionActor(competitionResult)), name = "competitionActor")

    actorRef ! 'START
  }

  /**
    * Stop competition
    */
  def stop = if (actorRef != null) actorRef ! 'DIE

  /**
    * Canvas.
    */
  object Canvas extends JPanel with Publisher {

    private var offScreenImageDrawed: Image = null
    private var offScreenGraphicsDrawed: Graphics = null

    private var evaderState: PEState = null
    private var pursuerState: PEState = null

    setPreferredSize(new Dimension(maxWidth, maxHeight))

    def terminate = publish(ReplayCompleteMessage)

    /**
      * Use double buffering.
      *
      * @see java.awt.Component#update(java.awt.Graphics)
      */
    override def update(g: Graphics) = {
      paint(g)
    }

    /**
      * Draw this generation.
      *
      * @see java.awt.Component#paint(java.awt.Graphics)
      */
    override def paint(g: Graphics) = {

      val d = getSize();

      if (offScreenImageDrawed == null) {
        // Double-buffer: clear the offscreen image.
        offScreenImageDrawed = createImage(d.width, d.height)
      }

      offScreenGraphicsDrawed = offScreenImageDrawed.getGraphics()
      offScreenGraphicsDrawed.setColor(Color.BLACK)
      offScreenGraphicsDrawed.fillRect(0, 0, d.width, d.height)

      // Paint offscreen
      renderOffScreen(offScreenImageDrawed.getGraphics());

      g.drawImage(offScreenImageDrawed, 0, 0, null);
    }

    // Repaint Offscreen graphics.
    def renderOffScreen(g: Graphics) {
      evaderTrace.paint(g)
      pursuerTrace.paint(g)
    }

    /**
      * Update evader/pursuer state and repaint.
      * Must be called on EDT.
      */
    def update(newEvaderState: PEState, newPursuerState: PEState) = {

      evaderTrace = evaderTrace.addState(newEvaderState)
      pursuerTrace = pursuerTrace.addState(newPursuerState)

      Canvas.repaint()
    }
  }

  peer.add(Canvas)
}