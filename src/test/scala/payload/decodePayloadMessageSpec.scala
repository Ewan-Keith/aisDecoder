package com.ewankeith.aisdecoder

import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class decodePayloadMessageSpec extends FlatSpec {

  private val validPayload = "14eG;o"
  private val validPayloadDecoded =
    Success("000001000100101101010111001011110111")
  private val invalidPayload = "14eZG;o"
  private val invalidPayloadMulti = "1Y4eZG;o"

  "toBitStream" should "correctly convert a valid payload message to bit stream" in {
    val result = decodePayloadMessage.toBitStream(validPayload)
    assert(result === validPayloadDecoded)
  }

  it should "return a Failure when containing an invalid character" in {
    val failureOutput = decodePayloadMessage.toBitStream(invalidPayload)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "Invalid AIS payload character found: Z")
      case Success(_) =>
        fail("toBitStream succeeded when payload contained invalid char")
    }
  }

  it should "return only the first Failure when multiple found" in {
    val failureOutput = decodePayloadMessage.toBitStream(invalidPayloadMulti)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "Invalid AIS payload character found: Y")
      case Success(_) =>
        fail("toBitStream succeeded when payload contained invalid char")
    }
  }
}