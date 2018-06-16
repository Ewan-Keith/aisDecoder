package com.ewankeith.aisdecoder

import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import scala.util.{ Try, Success, Failure }

class convertAisTests extends FlatSpec {

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

  "checkPayloadChar" should "return Success(char) for valid chars" in {
    validChars foreach { char =>
      val successful = convertAis.checkPayloadChar(validChars)(char)
      assert(successful === Success(char))
    }
  }

  it should "return Failure(x) for invalid chars" in {
    invalidChars foreach { char =>
      val failed = convertAis.checkPayloadChar(validChars)(char)
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
      val result = convertAis.charToSixBit(input)
      assert(result.isSuccess)
    }
  }

  it should "calculate correct character => six bit decmial values" in {
    val testResults = successCharacters.map(convertAis.charToSixBit)
    val comparisons = testResults zip successSixBitConversions
    comparisons foreach {
      case (test, correct) =>
        assert(test === correct)
    }
  }

  it should "return Failure(msg) when provided Failure(msg)" in {
    val failureInput = Failure(new Exception("test message"))
    val failureOutput = convertAis.charToSixBit(failureInput)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "test message")
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
      val result = convertAis.sixBitIntToBinary(input)
      assert(result.isSuccess)
    }
  }
  
    it should "calculate correct Int => bit stream Strings" in {
    val testResults = successInts.map(convertAis.sixBitIntToBinary)
    val comparisons = testResults zip successBitStreams
    comparisons foreach {
      case (test, correct) =>
        assert(test === correct)
    }
  }

  it should "return Failure(msg) when provided Failure(msg)" in {
    val failureInput = Failure(new Exception("test message"))
    val failureOutput = convertAis.sixBitIntToBinary(failureInput)
    failureOutput match {
      case Failure(e) =>
        assert(e.getMessage === "test message")
      case Success(_) =>
        fail("sixBitIntToBinary returned Success when input a Failure")
    }
  }

}