package com.matthewtyler.pe.gui

import org.jfree.chart.{ChartFactory,ChartPanel,JFreeChart}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries,XYSeriesCollection}

import java.awt.{Color,Dimension}

import scala.swing._

import com.matthewtyler.pe.logging.Logging

class PEWinPanel extends BoxPanel(Orientation.Vertical) with Logging {
 
  // Series
  val evaderWinsSeries = new XYSeries("Evader Wins") 
  val pursuerWinsSeries = new XYSeries("Pursuer Wins")
  
  // Data
  val winsData = new XYSeriesCollection(evaderWinsSeries)
  winsData.addSeries(pursuerWinsSeries)
  
  // Chart
  val winsChart = ChartFactory.createXYLineChart("Evader/Pursuer wins",
                                                 "Generation", 
                                                 "Wins", 
                                                 winsData,
                                                 PlotOrientation.VERTICAL,
                                                 true,
                                                 true,
                                                 false) 
                                                 
 // Set colours                                                
 val renderer = winsChart.getXYPlot.getRenderer
 renderer.setSeriesPaint(0, Color.BLUE);
 renderer.setSeriesPaint(1, Color.RED);                                                
                                                                                            
  var time = 0.0
  
  contents += new Panel() {
    val chartPanel = new ChartPanel(winsChart)
    chartPanel.setPreferredSize(new Dimension(1100,700))
    peer.add(chartPanel)
  }
    
  /**
   * Reset chart.
   */
  def reset = {
    time = 0.0;
    evaderWinsSeries.clear
    pursuerWinsSeries.clear
  }
  
  /**
   * Update wins.
   */
  def update(evaderWins : Int,pursuerWins : Int ) = {
    evaderWinsSeries.add(time,evaderWins)
    pursuerWinsSeries.add(time,pursuerWins)
    time += 1.0
  }
}