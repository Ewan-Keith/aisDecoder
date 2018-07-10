package com.ewankeith.aisdecoder.sentence.nmea.ais

import scala.util.{ Try, Success, Failure }
import com.ewankeith.aisdecoder.sentence.Sentence

case class AisSentence(sentence: String) extends Sentence(sentence) {

  // returns success(sentence) if valid, else returns informative failure
  override val isValid: Try[String] = AisSentenceValidity.tryValidity(Try(sentence))

  val fragmentCount: Try[Option[Int]] = this.asList match {
    case Success(list) => Try(list(1) match {
      case Some(str) => Some(str.toInt)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

  // get third field, the fragment number
  val fragmentNumber: Try[Option[Int]] = this.asList match {
    case Success(list) => Try(list(2) match {
      case Some(str) => Some(str.toInt)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

  // get fourth field, the sequential message ID
  val messageID: Try[Option[Int]] = this.asList match {
    case Success(list) => Try(list(3) match {
      case Some(str) => Some(str.toInt)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

  // get fifth field, the channel code
  val channelCode: Try[Option[String]] = this.asList match {
    case Success(list) => Try(list(4) match {
      case Some(str) => Some(str)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

  // get sixth field, the payload
  val payload: Try[Option[String]] = this.asList match {
    case Success(list) => Try(list(5) match {
      case Some(str) => Some(str)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

  // get seventh field, the padding
  val padding: Try[Option[Int]] = this.asList match {
    case Success(list) => Try(list(6) match {
      case Some(str) => Some(str.toInt)
      case None      => None
    })
    case Failure(e) => Failure(e)
  }

}
