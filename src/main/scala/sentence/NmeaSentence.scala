package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

case class NmeaSentence(sentence: String) {

  // defines a List representation of the string split by comma
  // TODO rather than wrap in a try the split should come after the fully composed nmea validity checks
    def asList: Try[List[String]] = Try(sentence.split(',').toList)

  //  // define NMEA sentence validity checks
  //  def isValid: Try[Boolean] = {
  //
  //    // Exception factory
  //    def NmeaExceptionFactory(error: String): Exception =
  //      new Exception(s"Invalid NMEA Sentence: $error")
  //
  //    // local list representation
  //    val localList = Try(sentence.split(',').toList)
  //
  //    localList match {
  //      case Success(list) => {
  //
  //        if (list.length != 7)
  //          Failure(NmeaExceptionFactory("Not 7 fields long"))
  //        else
  //          Try(true)
  //      }
  //      case Failure(msg) => Failure(msg)
  //    }
  //
  //  }

  // define the protocol of the sentence
  //  def protocol: Try[String] = {
  //    this.asList match {
  //      case Success(list) => Try(list.head)
  //      case Failure(msg) => Failure(msg)
  //    }
  //  }

  // method checks if sentence is an AIS protocol
  //  def isAis: Try[Boolean] = {
  //    this.asList match {
  //      case Success(list) => Try(List("!AIVDM", "!AIVDO").contains(list.head))
  //      case Failure(msg) => Failure(msg)
  //    }
  //  }

}
