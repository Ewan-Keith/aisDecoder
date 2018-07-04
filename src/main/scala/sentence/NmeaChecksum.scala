package com.ewankeith.aisdecoder.sentence

object NmeaChecksum {

  def isValid(sentence: String): Boolean = {
    val extractChecks = "!(.*)\\*([0-9a-fA-F]{2})$".r
    val extractChecks(trimmedSntnc, checkSum) = sentence

    val checksumCalc =
      trimmedSntnc.foldLeft(0)((a, b) => a ^ b).toHexString

    val checksumFormatted = String.format("%2s", checksumCalc)
      .replace(' ', '0').toUpperCase()

    checksumFormatted == checkSum
  }
}
