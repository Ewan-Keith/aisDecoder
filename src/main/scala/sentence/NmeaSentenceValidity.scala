package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object NmeaSentenceValidity {

  // Exception factory
  def NmeaExceptionFactory(error: String): Exception =
    new Exception(s"Invalid NMEA Sentence: $error")

  // define a Regex structure checker
  def checkStructure(sentence: String): Try[String] =
    if (sentence matches "^.*,.*,.*,.*,.*,.*,.*$")
      Try(sentence)
    else
      Failure(NmeaExceptionFactory("Invalid sentence structure"))

  // define NMEA sentence validity checks
  def checkVailidity(sentence: String): Try[Boolean] = {

    // local list representation
    val localList = Try(sentence.split(',').toList)

    localList match {
      case Success(list) => {

        if (list.length != 7)
          Failure(NmeaExceptionFactory("Not 7 fields long"))
        else
          Try(true)
      }
      case Failure(msg) => Failure(msg)
    }

  }

}