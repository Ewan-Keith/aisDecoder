package com.ewankeith.aisdecoder.payload

import scala.util.{ Try, Success, Failure }

object decodePayloadCharacter {

  // define valid AIS message payload characters
  private val validChars: List[Char] =
    ((48 to 87).map(_.toByte).toList :::
      (96 to 119).map(_.toByte).toList)
      .map(_.toChar)

  // check if message char is valid, if not return a Failure
  // curried so list of valid Chars can be fed in at call time
  def checkPayloadChar(valid: List[Char]): Char => Try[Char] =
    char => {
      if (valid.contains(char))
        Try(char)
      else
        Failure(new Exception(s"Invalid AIS payload character found: $char"))
    }

  // convert a character to 6 bit decimal representation
  def charToBitStream(char: Try[Char]): Try[List[Int]] =
    char match {
      case Success(char) => PayloadByte(char.toByte).asBitStream
      case Failure(msg)  => Failure(msg)
    }

  // compose converters, prevents multiple traversals of input String
  def charToBinary = charToBitStream _ compose checkPayloadChar(validChars)
}

