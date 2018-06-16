package com.ewankeith.aisdecoder

import scala.util.{ Try, Success, Failure }

object convertAis {

  // define valid AIS message payload characters
  private val validChars: List[Char] =
    ((48 to 87).map(_.toByte).toList :::
      (96 to 119).map(_.toByte).toList)
      .map(_.toChar)

  def messageToBitStream(message: String): Try[String] =
    errorCheck(
      message
        .toList
        .map(charToBinary)
        )

  // check if message char is valid, if not return a Failure
  // curried so list of valid Chars fed in at time of method call
  def checkPayloadChar(valid: List[Char]): Char => Try[Char] =
    char => {
      if (valid.contains(char))
        Try(char)
      else
        Failure(new Exception(s"Invalid AIS payload character found: $char"))
    }

  // convert a character to 6 bit decimal representation
  def charToSixBit(char: Try[Char]): Try[Int] =
    char match {
      case Success(char) => eightToSixBitCalculation(char.toByte)
      case Failure(msg)  => Failure(msg)
    }

  // carries out the numerical 8 bit to 6 bit decimal conversion
  private def eightToSixBitCalculation(eightBit: Byte): Try[Int] = {
    if (eightBit >= 48 && eightBit <= 87)
      Try(eightBit - 48)
    else if (eightBit >= 96 && eightBit <= 119)
      Try(eightBit - 56)
    else
      Failure(new Exception("Bit arithmetic error occured during payload conversion"))
  }

  // convert the decimal six bit value to a bit string
  def sixBitIntToBinary(decimalInt: Try[Int]): Try[String] =
    decimalInt match {
      case Success(decimalInt) => Try(
        String.format("%6s", decimalInt.toBinaryString).replace(' ', '0'))
      case Failure(msg) => Failure(msg)
    }

  // compose converters, prevents multiple traversals of input String
  private val charToBinary = 
    sixBitIntToBinary _ compose charToSixBit _ compose checkPayloadChar(validChars)

  // return first error or a single valid bit stream
  def errorCheck(bitStreams: List[Try[String]]): Try[String] = {
    val (successes, failures) = bitStreams.partition(_.isSuccess)

    // if no failures found, return flattened list, else return first error
    if (failures.isEmpty)
      Try(successes.map(_.get).mkString)
    else
      failures.head
  }

}