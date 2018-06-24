package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object NmeaSentenceValidity {

  // Exception factory
  def nmeaExceptionFactory(error: String): Exception =
    new Exception(s"Invalid NMEA Sentence: $error")

  // define validity check factory
  def validChkFactory(
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
//    sentence match {
//      case Success(sntnc) if (sntnc.split(',').head.tail
//          .toUpperCase().equals(sntnc.tail)) => Try(sntnc)
//      case Success(sntnc) if !(sntnc.split(',').head.tail
//          .toUpperCase().equals(sntnc.tail)) =>
//        Failure(nmeaExceptionFactory("First field is not upper case"))
//      case Failure(e) => Failure(e)
//    }
  
  // compose the identifier checks together
    def tryIdentifier(sentence: Try[String]) = 
      tryIdentifierLength _ compose tryStartCharacter _ compose tryIdentifierCase _
  

  // The 7 field length is AIS specific, split out to a AisSentenceValidity object
//  def checkVailidity(sentence: String): Try[Boolean] = {
//
//    // local list representation
//    val localList = Try(sentence.split(',').toList)
//
//    localList match {
//      case Success(list) => {
//
//        if (list.length != 7)
//          Failure(NmeaExceptionFactory("Not 7 fields long"))
//          else if ()
//        else
//          Try(true)
//      }
//      case Failure(msg) => Failure(msg)
//    }
//
//  }

}