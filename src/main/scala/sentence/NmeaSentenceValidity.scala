package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object NmeaSentenceValidity {

  // Exception factory
  def NmeaExceptionFactory(error: String): Exception =
    new Exception(s"Invalid NMEA Sentence: $error")

  // define a Regex structure checker
  def tryCommaStructure(sentence: Try[String]): Try[String] =
    sentence match {
      case Success(sntnc) if (sntnc matches "^(.*,)+.*$") =>
        Try(sntnc)
      case Success(sntnc) if !(sntnc matches "^(.*,)+.*$") =>
        Failure(NmeaExceptionFactory("Invalid comma structure"))
      case Failure(e) => Failure(e)
    }
      
  // check the identifier field begins with ! or $
  def tryStartCharacter(sentence: Try[String]): Try[String] = 
    sentence match {
      case Success(sntnc) if (List('!', '$').contains(sntnc.head)) =>
        Try(sntnc)
      case Success(sntnc) if !(List('!', '$').contains(sntnc.head)) =>
        Failure(NmeaExceptionFactory("Does not begin with '!' or '$'"))
      case Failure(e) => Failure(e)
    }
  
  // check the identifier field is 6 characters long
    def tryIdentifierLength(sentence: Try[String]): Try[String] = 
    sentence match {
      case Success(sntnc) if (sntnc.split(',').head.length == 6) =>
        Try(sntnc)
      case Success(sntnc) if !(sntnc.split(',').head.length == 6) =>
        Failure(NmeaExceptionFactory("First field not 6 characters long"))
      case Failure(e) => Failure(e)
    }
  
  // check the characters of the identifier field are all upper case
    def tryIdentifierCase(sentence: Try[String]): Try[String] = 
    sentence match {
      case Success(sntnc) if (sntnc.split(',').head.tail
          .toUpperCase().equals(sntnc.tail)) => Try(sntnc)
      case Success(sntnc) if !(sntnc.split(',').head.tail
          .toUpperCase().equals(sntnc.tail)) =>
        Failure(NmeaExceptionFactory("First field is not upper case"))
      case Failure(e) => Failure(e)
    }
  
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