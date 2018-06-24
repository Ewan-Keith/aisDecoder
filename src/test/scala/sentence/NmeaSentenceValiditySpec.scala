package com.ewankeith.aisdecoder.sentence

import scala.io.Source
import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class NmeaSentenceValiditySpec extends FlatSpec {

  val validAivdm: List[String] = Source
    .fromURL(getClass.getResource("/nmea-sample.txt"))
    .getLines.toList

  val invalidNmeaStruct: List[String] = Source
    .fromURL(getClass.getResource("/invalid-nmea-structures.txt"))
    .getLines.toList

  "NmeaExceptionFactory" should "return a custom NMEA exception" in {
    val testExcept = NmeaSentenceValidity.NmeaExceptionFactory("test message")
    assert(testExcept.isInstanceOf[Exception])
    assert(testExcept.getMessage === "Invalid NMEA Sentence: test message")
  }

  "checkStructure" should "return Success(msg) for valid sentence structure" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentenceValidity.checkStructure(sntnc) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("checkStructure returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(exc) for invalid sentence structure" in {
    invalidNmeaStruct.foreach(
      sntnc => {
        NmeaSentenceValidity.checkStructure(sntnc) match {
          case Success(msg) =>
            fail("checkStructure returned Success for invalid sentence")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid NMEA Sentence: Invalid sentence structure")
        }
      })
  }

}