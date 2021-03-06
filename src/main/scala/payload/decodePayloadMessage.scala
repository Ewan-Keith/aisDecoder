package com.ewankeith.aisdecoder.payload

import scala.util.{ Try, Success, Failure }

object decodePayloadMessage {

  // return the bit stream representation for an AIS payload string
  def toBitStream: String => Try[List[Int]] =
    errorCheck _ compose applyCharacterDecoder _

  // split the string and apply character level converter
  private def applyCharacterDecoder(message: String): List[Try[List[Int]]] =
    message
      .toList
      .map(decodePayloadCharacter.charToBinary)

  // return first error in results list, or a single valid bit stream
  private def errorCheck(bitStreams: List[Try[List[Int]]]): Try[List[Int]] = {
    val (successes, failures) = bitStreams.partition(_.isSuccess)

    // if no failures found, return flattened list, else return first error
    if (failures.isEmpty) { Try(successes.map(_.get).flatten) }
    else { failures.head }
  }

}
