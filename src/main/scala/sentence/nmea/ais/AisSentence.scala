package com.ewankeith.aisdecoder.sentence.nmea.ais

import scala.util.{ Try, Success, Failure }
import com.ewankeith.aisdecoder.sentence.Sentence

case class AisSentence(sentence: String) extends Sentence(sentence) {

  // returns success(sentence) if valid, else returns informative failure
  override val isValid: Try[String] = AisSentenceValidity.tryValidity(Try(sentence))

}
