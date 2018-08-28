package com.matthewtyler.pe.gui

import java.awt.Dimension

import com.matthewtyler.pe.logging.Logging
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scala.swing._

class StatsPanel(val description: String, plotColor: Color) extends BoxPanel(Orientation.Vertical) with Logging {

  // Series
  val fitnessSeries = new XYSeries("%s fitness".format(description))

  // Data
  val fitnessData = new XYSeriesCollection(fitnessSeries)

  // Charts
  val fitnessChart = ChartFactory.createXYAreaChart("%s fitness".format(description),
    "Generation",
    "Fitness",
    fitnessData,
    PlotOrientation.VERTICAL,
    false,
    true,
    false)

  // Set colour
  val renderer = fitnessChart.getXYPlot.getRenderer
  renderer.setSeriesPaint(0, plotColor)

  var time = 0.0

  contents += new Panel() {
    val chartPanel = new ChartPanel(fitnessChart)
    chartPanel.setPreferredSize(new Dimension(750, 450))
    peer.add(chartPanel)
  }

  /**
    * Reset chart.
    */
  def reset = {
    time = 0.0;
    fitnessSeries.clear
  }

  /**
    * Update series.
    */
  def update(fitness: Double) = {
    fitnessSeries.add(time, fitness)
    time += 1.0
  }
}