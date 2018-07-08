package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

// parent class for all NMEA sentence objects (high level and specific)
abstract class Sentence(sentence: String) {

  val isValid: Try[String]

  def asList: Try[List[String]] = isValid match {
    case Success(sntnc) => Try(sntnc.split(',').toList)
    case Failure(e)     => Failure(e)
  }

  def protocol: Try[String] = {
    this.asList match {
      case Success(list) => Try(list.head)
      case Failure(e)    => Failure(e)
    }
  }

}