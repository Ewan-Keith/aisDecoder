package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }

abstract class SentenceValidity {

  // validity factory requires a sentence specific exception to throw
  def exceptionFactory(error: String): Exception

  // factory for defining validity checks
  def validChkFactory(
    condition: String => Boolean, error: String): Try[String] => Try[String] = {
    sentence: Try[String] =>
      sentence match {
        case Success(sntnc) if (condition(sntnc)) =>
          Try(sntnc)
        case Success(sntnc) if !(condition(sntnc)) =>
          Failure(exceptionFactory(error))
        case Failure(e) => Failure(e)
      }
  }

  // checks the sentence checksum for validity
  def testChecksum(sentence: String): Boolean = {
    val extractChecks = "!(.*)\\*([0-9a-fA-F]{2})$".r
    val extractChecks(trimmedSntnc, checkSum) = sentence

    val checksumCalc =
      trimmedSntnc.foldLeft(0)((a, b) => a ^ b).toHexString

    val checksumFormatted = String.format("%2s", checksumCalc)
      .replace(' ', '0').toUpperCase()

    checksumFormatted == checkSum
  }

}
