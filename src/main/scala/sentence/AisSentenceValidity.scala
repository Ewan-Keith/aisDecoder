package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object AisSentenceValidity {

  // Exception factory
  private def aisExceptionFactory(error: String): Exception =
    new Exception(s"Invalid AIS Sentence: $error")

  // define validity check factory
  private def validChkFactory(
    condition: String => Boolean, error: String): Try[String] => Try[String] = {
    sentence: Try[String] =>
      sentence match {
        case Success(sntnc) if (condition(sntnc)) =>
          Try(sntnc)
        case Success(sntnc) if !(condition(sntnc)) =>
          Failure(aisExceptionFactory(error))
        case Failure(e) => Failure(e)
      }
  }

  // check that the sentence has 7 fields
  def tryLengthSeven(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x.split(',').length.equals(7),
      "Number of fields != 7")(sentence)

  // check that the first field is AIVDM/O

  // check that checksum is used (mandatory for AIS messages)

  // check that checksum is valid

  // check that field 3 isn't greater than field 2

  // check that field 5 is 'A', 'B', '1' or '2' or empty (all appear in the wild)

}