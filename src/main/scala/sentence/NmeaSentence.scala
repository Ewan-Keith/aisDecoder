package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

// class is concrete allowing all nmea sentences a common entry point
// has a method that attempts to raise NmeaSentence to a specific protocol
case class NmeaSentence(sentence: String) extends Sentence(sentence) {

  // returns success(sentence) if valid, else returns informative failure
  override val isValid: Try[String] = NmeaSentenceValidity.tryValidity(Try(sentence))

  // use the protocol to try and construct a specific sentence object.
  val raiseProtocol: Try[Sentence] = {
    this.isValid match {
      case Success(sntnc) if AisSentenceValidity.validIdent contains this.protocol =>
        Try(AisSentence(sntnc))
      case Failure(e) => Failure(e)
      case _ =>
        Failure(new Exception("Failed to identify NMEA sentence protocol"))
    }
  }

}
