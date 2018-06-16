package com.ewankeith.aisdecoder

import scala.util.{ Try, Success, Failure }

object convertAis {

  def messageToBitStream(message: String): Try[String] =
    errorCheck(
      message
        .toList
        .map(decodePayloadCharacter.charToBinary)
        )


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