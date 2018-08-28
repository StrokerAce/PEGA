package com.matthewtyler.pe.gui

import scala.swing._
import scala.swing.Dialog._

import com.matthewtyler.pe.logging.Logging

/**
 * GUI Helper functions
 */
object GuiHelper extends Logging {
 
    /**
     * Add item to ListView
     */
    def addToListView[T](listView : ListView[T],item : T) = {
      listView.listData = listView.listData.toList ++ List(item)
    }

    /**
     * Add item to ListViews
     */
    def addToListViews[T](listViews : List[ListView[T]],item : T) = {
      listViews.map(addToListView(_,item))
    }   
  
  /**
   * Get converted + validated value or throw.
   */
  def getOrThrow[T](either : Either[String,T]) = either match {
    case Left(message) => throw new IllegalArgumentException("Unable to process. Reason %s".format(message))
    case Right(value) => value
  }
  
  /**
   * Parse function.
   */
  private def parse[T](conversion :  String => T)(value : String): Either[String,T] = {   
    try {
      Right(conversion(value))
    }
    catch {
      case e: Throwable => Left(e.getMessage())
    }
  }
  
  /**
   * Parse Int
   */
  def parseInt(value : String) = parse(Integer.parseInt)(value)
  
  /**
   * Parse Long
   */
  def parseLong(value : String) = parse(java.lang.Long.parseLong)(value)
  
  /**
   * Parse Double
   */
  def parseDouble(value : String) = parse(java.lang.Double.parseDouble)(value)
  
  /**
   * Parse input and validate parsed value satisfies validation function.
   */
  def convertAndValidate[T](value : Either[String,T])(validate : T => Either[String,T]): Either[String,T] = value match {
    case Left(message) => Left("Error parsing input %s".format(value))
    case Right(value) => validate(value)
  }
  
  /**
   * Parse and validate TextField.
   * 
   * If validation fails display error and reset to default.
   */
  def processTextFieldUpdate(component  : Component,
                             textField  : TextField,
                             default    : String,
                             validation : Either[String,_],
                             title      : String) = validation match {
     
    case Left(message) =>
      Dialog.showMessage(component,message,title,Message.Error)
      textField.text = default
    case Right(value) =>
      debug("Set {} to {}",title,value)
  }
  
  /**
   * Update input fields with the supplied function.
   */
  def updateFields(fields : List[TextField],operation : TextField => Unit) = fields.map(operation)
}