package com.ewankeith.aisdecoder.payload

import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class PayloadByteSpec extends FlatSpec {
  
  val validByte = PayloadByte(56.toByte)
  val invalidByte = PayloadByte(5.toByte)
  
  "PayloadByte" should "store the original byte value" in {
    assert(validByte.eightBitDecimal === 56.toByte)
    assert(invalidByte.eightBitDecimal === 5.toByte)
  }
  
  it should "correctly convert valid byte to six bit decimal [Int]" in {
    assert(validByte.asSixBitDecimal === Success(8))
  }

  "asSixBitDecimal" should "return correct Failure for invalid byte" in {
    invalidByte.asSixBitDecimal match {
      case Failure(e) =>
        assert(e.getMessage === "Invalid payload byte in PayloadBit conversion: 5")
      case Success(_) =>
        fail("asSixBitDecimal returned Success for an invalid byte")
    }
  }
  
  it should "return correct six bit decimal for valid byte" in {
    validByte.asSixBitDecimal match {
      case Success(sbd) => assert(sbd === 8)
      case Failure(_) => 
        fail("asSixBitDecimal returned Failure for a valid byte")
    }
  }
  
    "asBitStream" should "return correct Failure for invalid byte" in {
    invalidByte.asBitStream match {
      case Failure(e) =>
        assert(e.getMessage === "Invalid payload byte in PayloadBit conversion: 5")
      case Success(_) =>
        fail("asSixBitDecimal returned Success for an invalid byte")
    }
  }
    
      it should "return correct bit stream for valid byte" in {
    validByte.asBitStream match {
      case Success(bitStream) => assert(bitStream === List(0, 0, 1, 0, 0, 0))
      case Failure(_) => 
        fail("asBitStream did not return correct bitStream")
    }
  }
  
}