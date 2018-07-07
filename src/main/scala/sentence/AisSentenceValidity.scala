package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object AisSentenceValidity {

  // valid field length of AIS messages
  private val validLength = 7

  // define all valid identifier for AIS messages
  private val validIdent = for {
    starts <- List("!AD", "!AI", "!AN", "!AR", "!AS", "!AT", "!AX", "!BS", "!SA")
    ends <- List("VDM", "VDO")
  } yield starts + ends

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
      x => x.split(',').length.equals(validLength),
      s"Number of fields != $validLength")(sentence)

  // check that the first field is AIVDM/O
  def tryValidIdent(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => validIdent.contains(x.split(',').head),
      "Not valid AIS identifier")(sentence)

  // check that checksum is used (mandatory for AIS messages)
  def tryChecksumPresent(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x.matches("^.*\\*[0-9a-fA-F]{2}$"),
      "No valid checksum provided")(sentence)

  // check that checksum is valid
  def tryChecksumValid(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => NmeaChecksum.isValid(x),
      "Checksum is invalid")(sentence)

  // check that field 3 isn't greater than field 2
  def tryFieldThreeLteTwo(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => x.split(',')(2) <= x.split(',')(1),
      "Field 3 greater than field 2")(sentence)

  // check that field 5 is 'A', 'B', '1', '2' or empty (all appear in the wild)
  def tryFieldFiveValid(sentence: Try[String]): Try[String] =
    validChkFactory(
      x => List("A", "B", "1", "2", "").contains(x.split(',')(4)),
      "Field 5 is not 'A', 'B', '1', '2' or empty")(sentence)

  // compose checksum checks into one test
  def tryChecksum: Try[String] => Try[String] =
    tryChecksumValid _ compose tryChecksumPresent _
  
    
    // compose checksum checks with rest of the AIS specific checks
  def tryValidity: Try[String] => Try[String] = {
    tryChecksum compose tryFieldFiveValid _ compose 
    tryFieldThreeLteTwo _ compose tryValidIdent _ compose tryLengthSeven _
  }
}
