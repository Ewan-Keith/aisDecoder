package com.ewankeith.aisdecoder.sentence.nmea.ais

import scala.util.{ Try, Success, Failure }
import com.ewankeith.aisdecoder.sentence.Sentence

case class AisSentence(sentence: String) extends Sentence(sentence) {

  // returns success(sentence) if valid, else returns informative failure
  override val isValid: Try[String] = AisSentenceValidity.tryValidity(Try(sentence))

  // get second field, the fragment count
  val fragmentCount: Try[Int] = this.asList match {
    case Success(list) => Try(list(1).toInt)
    case Failure(e)    => Failure(e)
  }

  // get third field, the fragment number
  val fragmentNumber: Try[Int] = this.asList match {
    case Success(list) => Try(list(2).toInt)
    case Failure(e)    => Failure(e)
  }

  // get fourth field, the sequential message ID
  val messageID: Try[Int] = this.asList match {
    case Success(list) => Try(list(3).toInt)
    case Failure(e)    => Failure(e)
  }

  // get fifth field, the channel code
  val channelCode: Try[Int] = this.asList match {
    case Success(list) => Try(list(4).toInt)
    case Failure(e)    => Failure(e)
  }
  
    // get sixth field, the payload
  val payload: Try[Int] = this.asList match {
    case Success(list) => Try(list(5).toInt)
    case Failure(e)    => Failure(e)
  }
}
