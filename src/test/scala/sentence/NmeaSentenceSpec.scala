package com.ewankeith.aisdecoder.sentence

import scala.io.Source
import org.scalatest.FlatSpec
import scala.util.{ Try, Success, Failure }

class NmeaSentenceSpec extends FlatSpec {

  val validAivdm: List[String] = Source
    .fromURL(getClass.getResource("/nmea-sample.txt"))
    .getLines.toList

  "NmeaSentence" should "define a 7 item list for valid sentences" in {

    validAivdm.foreach(
      sntnc => {
        NmeaSentence(sntnc).asList match {
          case Success(list) => assert(list.length === 7)
          case Failure(msg) =>
            fail("NmeaSentence.asList returned Failure for valid sentence")
        }
      })

  }
}