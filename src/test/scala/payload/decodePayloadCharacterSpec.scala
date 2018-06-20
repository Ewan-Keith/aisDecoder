package com.ewankeith.aisdecoder.payload

import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class decodePayloadCharacterSpec extends FlatSpec {

  // define valid AIS message payload characters
  private val validChars: List[Char] =
    ((48 to 87).map(_.toByte).toList :::
      (96 to 119).map(_.toByte).toList)
      .map(_.toChar)

  // define invalid AIS message payload characters
  private val invalidChars: List[Char] =
    ((32 to 47).map(_.toByte).toList :::
      (88 to 95).map(_.toByte).toList)
      .map(_.toChar)

  "checkPayloadChar" should "return Success[char] for valid chars" in {
    validChars foreach { char =>
      val successful = decodePayloadCharacter.checkPayloadChar(validChars)(char)
      assert(successful === Success(char))
    }
  }

  it should "return Failure(x) for invalid chars" in {
    invalidChars foreach { char =>
      val failed = decodePayloadCharacter.checkPayloadChar(validChars)(char)
      failed match {
        case Failure(e) =>
          assert(e.getMessage === s"Invalid AIS payload character found: $char")
        case Success(_) =>
          fail("checkAisChar returned Success for an invalid character: $char")
      }
    }
  }

  // define Success[Char] values
  private val successCharacters = List('0', '7', '<', '?', '@', '`', 'q')
    .map(Success(_))

  // define Success[Int] 6 bit decimals for successCharacters list
  private val successSixBitConversions = List(0, 7, 12, 15, 16, 40, 57)
    .map(Success(_))
  

  // define valid Success six bit integers
  private val successInts = List(0, 13, 39, 40, 52, 63)
  .map(Success(_))

  // define correct Success bit streams for successInts
  private val successBitStreams =
    List("000000", "001101", "100111", "101000", "110100", "111111")
      .map(Success(_))
  

  "charToBinary" should "return Success[List[Int]] for valid chars" in {
    val result = decodePayloadCharacter.charToBinary('K')
    assert(result === Success(List(0, 1, 1, 0, 1, 1)))
  }

  it should "return Failure(x) for invalid chars" in {
    invalidChars foreach { char =>
      val failed = decodePayloadCharacter.charToBinary(char)
      failed match {
        case Failure(e) =>
          assert(e.getMessage === s"Invalid AIS payload character found: $char")
        case Success(_) =>
          fail("checkAisChar returned Success for an invalid character: $char")
      }
    }
  }

}