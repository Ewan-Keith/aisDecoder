package com.ewankeith.aisdecoder.sentence.nmea

import scala.util.{ Try, Success, Failure }
import com.ewankeith.aisdecoder.sentence.SentenceValidity

object NmeaSentenceValidity extends SentenceValidity {

  // Exception factory
  override def exceptionFactory(error: String): Exception =
    new Exception(s"Invalid NMEA Sentence: $error")

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
