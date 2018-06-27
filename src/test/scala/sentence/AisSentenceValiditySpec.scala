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
    
      val invalidLength = List("a,b,c,d,e,f", "a,v,d", "1,2,3,4,5,6,7,8")
      
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
  
}