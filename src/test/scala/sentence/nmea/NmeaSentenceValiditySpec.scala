package com.ewankeith.aisdecoder.sentence.nmea

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

  val testFailure = Failure(
    new Exception("Invalid NMEA Sentence: test message"))

  val invalidCommaStruct = List("abcd", "!AIVDM;123", "1:2:3:4:5")

  "tryCommaStructure" should "return Success(msg) for valid comma structure" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentenceValidity.tryCommaStructure(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryCommaStructure returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(e) for invalid comma structure" in {
    invalidCommaStruct.foreach(
      sntnc => {
        NmeaSentenceValidity.tryCommaStructure(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryCommaStructure returned Success for invalid structure")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid NMEA Sentence: Invalid comma structure")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedStructure = NmeaSentenceValidity.tryCommaStructure(testFailure)

    triedStructure match {
      case Success(msg) =>
        fail("tryCommaStructure returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }

  val validStart = List("!test", "$test")
  val invalidStart = List("1234", "test1", "^$test", "'!test")

  "tryStartCharacter" should "return Success(msg) for valid start character" in {
    validStart.foreach(
      sntnc => {
        NmeaSentenceValidity.tryStartCharacter(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryStartCharacter returned Failure for valid start character")
        }
      })
  }

  it should "return Failure(e) for invalid start character" in {
    invalidStart.foreach(
      sntnc => {
        NmeaSentenceValidity.tryStartCharacter(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryStartCharacter returned Success for invalid start char")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid NMEA Sentence: Does not begin with '!' or '$'")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedStart = NmeaSentenceValidity.tryStartCharacter(testFailure)

    triedStart match {
      case Success(msg) =>
        fail("tryStartCharacter returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }

  val validIdentLength = List("!AIVDM", "$AIVDM", "test12")
  val invalidIdentLength = List("AIVDM", "!AIVD", "$AIVDMO", "")

  "tryIdentifierLength" should "return Success(msg) for valid ident length" in {
    validIdentLength.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifierLength(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryIdentifierLength returned Failure for valid identifier length")
        }
      })
  }

  it should "return Failure(e) for invalid identifier length" in {
    invalidIdentLength.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifierLength(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryIdentifierLength returned Success for invalid ident length")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid NMEA Sentence: First field not 6 characters long")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedLength = NmeaSentenceValidity.tryIdentifierLength(testFailure)

    triedLength match {
      case Success(msg) =>
        fail("tryIdentifierLength returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }

  val validIdentCase = List("!AIVDM", "$!AIVDM")
  val invalidIdentCase = List("!AIVDm", "!aIVDM", "$AiVDM")

  "tryIdentifierCase" should "return Success(msg) for valid ident case" in {
    validIdentCase.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifierCase(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryIdentifierCase returned Failure for valid identifier case")
        }
      })
  }

  it should "return Failure(e) for invalid identifier case" in {
    invalidIdentCase.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifierCase(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryIdentifierCase returned Success for invalid ident case")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid NMEA Sentence: First field is not upper case")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedCase = NmeaSentenceValidity.tryIdentifierCase(testFailure)

    triedCase match {
      case Success(msg) =>
        fail("tryIdentifierCase returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }

  val invalidIdentCollection = invalidStart :::
    invalidIdentLength ::: invalidIdentCase

  "tryIdentifier" should "return Success(msg) for valid identifier" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifier(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryIdentifier returned Failure for valid identifier")
        }
      })
  }

  it should "return Failure(e) for invalid nmea identifiers" in {
    invalidIdentCollection.foreach(
      sntnc => {
        NmeaSentenceValidity.tryIdentifier(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryIdentifier returned Success for invalid identifier")
          case Failure(e) =>
            assert(e.getMessage.startsWith(
              "Invalid NMEA Sentence: "))
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedCase = NmeaSentenceValidity.tryIdentifier(testFailure)

    triedCase match {
      case Success(msg) =>
        fail("tryIdentifier returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }

  val invalidNmeaCollection = invalidIdentCollection ::: invalidNmeaStruct

  "tryValidity" should "return Success(msg) for valid NMEA sentence" in {
    validAivdm.foreach(
      sntnc => {
        NmeaSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryValidity returned Failure for valid NMEA sentence")
        }
      })
  }

  it should "return Failure(e) for invalid nmea sentences" in {
    invalidNmeaCollection.foreach(
      sntnc => {
        NmeaSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid identifier")
          case Failure(e) =>
            assert(e.getMessage.startsWith(
              "Invalid NMEA Sentence: "))
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedCase = NmeaSentenceValidity.tryValidity(testFailure)

    triedCase match {
      case Success(msg) =>
        fail("tryValidity returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid NMEA Sentence: test message")
    }
  }
}
