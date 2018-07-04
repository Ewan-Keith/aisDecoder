package com.ewankeith.aisdecoder.sentence

import scala.io.Source
import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class AisSentenceValiditySpec extends FlatSpec {

  val validAivdm: List[String] = Source
    .fromURL(getClass.getResource("/nmea-sample.txt"))
    .getLines.toList

  val testFailure = Failure(
    new Exception("Invalid AIS Sentence: test message"))

  val invalidLength = List("a,b,c,d,e,f", "a,v,d", "1,2,3,4,5,6,7,8", "test")

  "tryLengthSeven" should "return Success(msg) for 7 field sentences" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryLengthSeven(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryCommaStructure returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(e) for non-7 field messages" in {
    invalidLength.foreach(
      sntnc => {
        AisSentenceValidity.tryLengthSeven(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryLengthSeven returned Success for invalid length")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Number of fields != 7")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedLength = AisSentenceValidity.tryLengthSeven(testFailure)

    triedLength match {
      case Success(msg) =>
        fail("tryLengthSeven returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  val invalidIdent =
    List("test,123,456", "!AIVD0,123,456", "!AVDM,1,1", "$AIVDM,1")

  "tryValidIdent" should "return Success(msg) for message with valid ident" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryValidIdent(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryValidIdent returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(e) for invalid ident messages" in {
    invalidIdent.foreach(
      sntnc => {
        AisSentenceValidity.tryValidIdent(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidIdent returned Success for invalid identifier")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Not valid AIS identifier")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedIdent = AisSentenceValidity.tryValidIdent(testFailure)

    triedIdent match {
      case Success(msg) =>
        fail("tryValidIdent returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  val absentChecksum = List("test*O2", "test02", "test*0", "123*012")

  "tryChecksumPresent" should "return Success(msg) for message with nmea checksum" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksumPresent(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryChecksumPresent returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(e) for  mis-formatted or missing checksum" in {
    absentChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksumPresent(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryChecksumPresent returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: No valid checksum provided")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedChksmPresent = AisSentenceValidity.tryChecksumPresent(testFailure)

    triedChksmPresent match {
      case Success(msg) =>
        fail("tryChecksumPresent returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  val invalidChecksum = List(
    "!AIVDM,1,1,,A,13HOI:0P0000VOHLCnHQKwvL05Ip,0*24",
    "!AIVDM,1,1,,A,133w;`PP00PCqghMcqNqdOvPR5Ip,0*00")

  "tryChecksumValid" should "return Success(msg) for valid nmea checksum" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksumValid(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryChecksumValid returned Failure for valid checksum")
        }
      })
  }

  it should "return Failure(e) for invalid checksum" in {
    invalidChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksumValid(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryChecksumValid returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Checksum is invalid")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedChksmValid = AisSentenceValidity.tryChecksumValid(testFailure)

    triedChksmValid match {
      case Success(msg) =>
        fail("tryChecksumValid returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }
  
}