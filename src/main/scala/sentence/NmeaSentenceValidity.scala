package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object NmeaSentenceValidity {

  // Exception factory
  private def nmeaExceptionFactory(error: String): Exception =
    new Exception(s"Invalid NMEA Sentence: $error")

  // define validity check factory
  private def validChkFactory(
    condition: String => Boolean, error: String): Try[String] => Try[String] = {
    sentence: Try[String] =>
      sentence match {
        case Success(sntnc) if (condition(sntnc)) =>
          Try(sntnc)
        case Success(sntnc) if !(condition(sntnc)) =>
          Failure(nmeaExceptionFactory(error))
        case Failure(e) => Failure(e)
      }
  }

  // define a comma structure checker
  def tryCommaStructure(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x matches "^(.*,)+.*$",
      "Invalid comma structure")(sentence)

  // check the identifier field begins with ! or $
  def tryStartCharacter(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => List('!', '$').contains(x.head),
      "Does not begin with '!' or '$'")(sentence)

  // check the identifier field is 6 characters long
  def tryIdentifierLength(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x.split(',').head.length == 6,
      "First field not 6 characters long")(sentence)

  // check the characters of the identifier field are all upper case
  def tryIdentifierCase(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x.split(',').head.tail.toUpperCase().equals(x.split(',').head.tail),
      "First field is not upper case")(sentence)

  // compose the identifier checks together
  def tryIdentifier: Try[String] => Try[String] =
    tryIdentifierCase _ compose tryStartCharacter _ compose tryIdentifierLength _

  // compose identifier and comma structure checks together
  def tryValidity: Try[String] => Try[String] =
    tryIdentifier compose tryCommaStructure _
}
