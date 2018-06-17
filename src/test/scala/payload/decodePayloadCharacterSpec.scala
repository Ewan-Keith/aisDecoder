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

  "charToSixBit" should "return Success[Int] with input Success[char]" in {
    successCharacters foreach { input =>
      val result = decodePayloadCharacter.charToSixBit(input)
      assert(result.isSuccess)
    }
  }

  it should "calculate correct character => six bit decmial values" in {
    val testResults = successCharacters.map(decodePayloadCharacter.charToSixBit)
    val comparisons = testResults zip successSixBitConversions
    comparisons foreach {
      case (test, correct) =>
        assert(test === correct)
    }
  }

  it should "return Failure(msg) when provided Failure(msg)" in {
    val failureInput = Failure(new Exception("test message"))
    val failureOutput = decodePayloadCharacter.charToSixBit(failureInput)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "test message")
      case Success(_) =>
        fail("charToSixBit returned Success when input a Failure")
    }
  }
  
  it should "return Failure if an invalid character is provided" in {
    val failureOutput = decodePayloadCharacter.charToSixBit(Success('Z'))
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === 
          "Bit arithmetic error occured during payload conversion")
      case Success(_) =>
        fail("charToSixBit returned Success when input a Failure")
    }
  }
  
  
  // define valid Success six bit integers
  private val successInts = List(0, 13, 39, 40, 52, 63)
  .map(Success(_))

  // define correct Success bit streams for successInts
  private val successBitStreams =
    List("000000", "001101", "100111", "101000", "110100", "111111")
      .map(Success(_))

  "sixBitIntToBinary" should "return Success[String] with input Success[Int]" in {
    successInts foreach { input =>
      val result = decodePayloadCharacter.sixBitIntToBinary(input)
      assert(result.isSuccess)
    }
  }
  
    it should "calculate correct Int => bit stream Strings" in {
    val testResults = successInts.map(decodePayloadCharacter.sixBitIntToBinary)
    val comparisons = testResults zip successBitStreams
    comparisons foreach {
      case (test, correct) =>
        assert(test === correct)
    }
  }

  it should "return Failure(msg) when provided Failure(msg)" in {
    val failureInput = Failure(new Exception("test message"))
    val failureOutput = decodePayloadCharacter.sixBitIntToBinary(failureInput)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "test message")
      case Success(_) =>
        fail("sixBitIntToBinary returned Success when input a Failure")
    }
  }
  

  "charToBinary" should "return Success[String] for valid chars" in {
    val result = decodePayloadCharacter.charToBinary('K')
    assert(result === Success("011011"))
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