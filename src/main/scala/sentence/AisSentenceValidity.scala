package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

object AisSentenceValidity {
  
    // Exception factory
  def aisExceptionFactory(error: String): Exception =
    new Exception(s"Invalid AIS Sentence: $error")
  
  
  
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