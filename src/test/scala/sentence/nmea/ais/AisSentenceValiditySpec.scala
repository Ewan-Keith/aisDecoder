package com.ewankeith.aisdecoder.sentence.nmea.ais

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
    List(
      "!IVDM,1,1,,B,13cUpi0qiM0g6jpHrSE1F@rP0d0q,0*3A",
      ",1,1,,B,13PfTH002D05e0rM6oN8Oo6R05Ip,0*43",
      "$AIVDM,1,1,,B,19NP0102BB2>ctkfmaB@0c>P0000,0*15",
      "55,1,1,,B,239K6?50010CTGTMH5gad:TR08:O,0*7C")

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

  val absentChecksum = List(
    "!AIVDM,1,1,,B,13cUpi0qiM0g6jpHrSE1F@rP0d0q,0*3AW",
    "!AIVDM,1,1,,B,13PfTH002D05e0rM6oN8Oo6R05Ip,0*",
    "!AIVDM,1,1,,B,19NP0102BB2>ctkfmaB@0c>P0000,0*1",
    "!AIVDM,1,1,,B,239K6?50010CTGTMH5gad:TR08:O,0*ZZ")

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

  "tryChecksum" should "return Success(msg) for message with valid nmea checksum" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksum(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryChecksum returned Failure for valid sentence")
        }
      })
  }

  it should "return Failure(e) for  mis-formatted or missing checksum" in {
    absentChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksum(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryChecksum returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: No valid checksum provided")
        }
      })
  }

  it should "return Failure(e) for invalid checksum" in {
    invalidChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryChecksum(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryChecksum returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Checksum is invalid")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedChksm = AisSentenceValidity.tryChecksum(testFailure)

    triedChksm match {
      case Success(msg) =>
        fail("tryChecksum returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  val invalidFieldNumbers = List(
    "!AIVDM,0,1,,B,35Mtp?0016J5ohD?ofRWSF2R0000,0*28",
    "!AIVDM,1,2,,A,133REv0P00P=K?TMDH6P0?vN289>,0*46",
    "!AIVDM,0,3,,B,139eb:PP00PIHDNMdd6@0?vN2D2s,0*43")

  "tryFieldThreeLteTwo" should "return Success(msg) for valid field numbers" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryFieldThreeLteTwo(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) =>
            fail("tryFieldThreeLteTwo returned Failure for valid field numbers")
        }
      })
  }

  it should "return Failure(e) for invalid field numbers" in {
    invalidFieldNumbers.foreach(
      sntnc => {
        AisSentenceValidity.tryFieldThreeLteTwo(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryFieldThreeLteTwo returned Success for invalid field numbers")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Field 3 greater than field 2")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedFieldNum = AisSentenceValidity.tryFieldThreeLteTwo(testFailure)

    triedFieldNum match {
      case Success(msg) =>
        fail("tryFieldThreeLteTwo returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  val invalidFifthFields = List(
    "!AIVDM,1,1,,C,13cUpi0qiM0g6jpHrSE1F@rP0d0q,0*3A",
    "!AIVDM,1,1,,a,13PfTH002D05e0rM6oN8Oo6R05Ip,0*43",
    "!AIVDM,1,1,,3,19NP0102BB2>ctkfmaB@0c>P0000,0*15",
    "!AIVDM,1,1,,0,239K6?50010CTGTMH5gad:TR08:O,0*7C",
    "!AIVDM,1,1,,TEST,14eGqlPP00L>h5tK=f7=5wwL05Ip,0*18")

  "tryFieldFiveValid" should "return Success(msg) for valid fifth field" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryFieldFiveValid(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) => {
            println(sntnc)
            fail("tryFieldFiveValid returned Failure for valid field numbers")
          }
        }
      })
  }

  it should "return Failure(e) for invalid fifth field" in {
    invalidFifthFields.foreach(
      sntnc => {
        AisSentenceValidity.tryFieldFiveValid(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryFieldFiveValid returned Success for invalid fifth field")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Field 5 is not 'A', 'B', '1', '2' or empty")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedFifthField = AisSentenceValidity.tryFieldFiveValid(testFailure)

    triedFifthField match {
      case Success(msg) =>
        fail("triedFifthField returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

  "tryValidity" should "return Success(msg) for valid fifth field" in {
    validAivdm.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) => assert(msg === sntnc)
          case Failure(e) => {
            println(sntnc)
            fail("tryValidity returned Failure for valid field numbers")
          }
        }
      })
  }

  it should "return Failure(e) for non-7 field messages" in {
    invalidLength.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid length")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Number of fields != 7")
        }
      })
  }

  it should "return Failure(e) for invalid ident messages" in {
    invalidIdent.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid identifier")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Not valid AIS identifier")
        }
      })
  }

  it should "return Failure(e) for  mis-formatted or missing checksum" in {
    absentChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: No valid checksum provided")
        }
      })
  }

  it should "return Failure(e) for invalid checksum" in {
    invalidChecksum.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid or missing checksum")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Checksum is invalid")
        }
      })
  }

  it should "return Failure(e) for invalid field numbers" in {
    invalidFieldNumbers.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid field numbers")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Field 3 greater than field 2")
        }
      })
  }

  it should "return Failure(e) for invalid fifth field" in {
    invalidFifthFields.foreach(
      sntnc => {
        AisSentenceValidity.tryValidity(Try(sntnc)) match {
          case Success(msg) =>
            fail("tryValidity returned Success for invalid fifth field")
          case Failure(e) =>
            assert(e.getMessage ===
              "Invalid AIS Sentence: Field 5 is not 'A', 'B', '1', '2' or empty")
        }
      })
  }

  it should "pass on Failure(e) unchanged" in {
    val triedOverall = AisSentenceValidity.tryValidity(testFailure)

    triedOverall match {
      case Success(msg) =>
        fail("tryValidity returned Success when input Failure(e)")
      case Failure(e) => assert(e.getMessage ===
        "Invalid AIS Sentence: test message")
    }
  }

}
