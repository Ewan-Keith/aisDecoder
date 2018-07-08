package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

case class AisSentence(sentence: String) extends Sentence(sentence) {

  // returns success(sentence) if valid, else returns informative failure
  override val isValid: Try[String] = AisSentenceValidity.tryValidity(Try(sentence))

}