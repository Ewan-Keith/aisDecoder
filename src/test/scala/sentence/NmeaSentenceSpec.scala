package com.ewankeith.aisdecoder.sentence

import scala.io.Source
import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class NmeaSentenceSpec extends FlatSpec {

  val validAivdm: List[String] = Source
    .fromURL(getClass.getResource("/nmea-sample.txt"))
    .getLines.toList

  val invalidCommaStruct = List(
    "!AIVDM11A133w;`PP00PCqghMcqNqdOvPR5Ip0*65",
    "!AIVDM;1;1;;B;35Mtp?0016J5ohD?ofRWSF2R0000;0*28")

  "isValid" should "return Success(sntnc) for valid sentence" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentence(sntnc).isValid match {
          case Success(msg) => assert(sntnc === msg)
          case Failure(e) =>
            fail("NmeaSentence.isValid returned Failure for valid sentence")
        }
      })
  }

  it should "return failure with invalid comma structure" in {
    invalidCommaStruct.foreach(
      sntnc => {
        NmeaSentence(sntnc).isValid match {
          case Success(list) =>
            fail("NmeaSentence.isValid returned Success for invalid comma structure")
          case Failure(e) => assert(e.getMessage ===
            "Invalid NMEA Sentence: Invalid comma structure")
        }
      })
  }

  "asList" should "define a 7 item list for valid AIS sentences" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentence(sntnc).asList match {
          case Success(list) => assert(list.length === 7)
          case Failure(msg) =>
            fail("NmeaSentence.asList returned Failure for valid sentence")
        }
      })
  }

  it should "return failure with invalid comma structure" in {
    invalidCommaStruct.foreach(
      sntnc => {
        NmeaSentence(sntnc).asList match {
          case Success(list) =>
            fail("NmeaSentence.asList returned Success for invalid comma structure")
          case Failure(e) => assert(e.getMessage ===
            "Invalid NMEA Sentence: Invalid comma structure")
        }
      })
  }

}
