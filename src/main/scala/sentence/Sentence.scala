package com.ewankeith.aisdecoder.sentence

import scala.util.{ Try, Success, Failure }
import scala.annotation.tailrec

// parent class for all NMEA sentence objects (high level and specific)
abstract class Sentence(sentence: String) {

  val isValid: Try[String]

  // sentence as list of values and optional checksum (Option wrapped)
  def asList: Try[List[Option[String]]] = {

    def checksumSplit(tail: String): List[String] = {
      val temp = "(^[0-9])\\*?([0-9a-fA-F]{2})?$".r
      val temp(padding, checksum) = tail
      List(padding, checksum)
    }

    val emptyStringIsNull: (String) => Option[String] = {
      case ""   => None
      case null => None
      case x    => Some(x)
    }

    def optioniseWithChecksum(sentence: List[String]): List[Option[String]] =
      sentence match {
        case x :: Nil => checksumSplit(x).map(emptyStringIsNull)
        case x :: xs  => emptyStringIsNull(x) :: optioniseWithChecksum(xs)
        case Nil      => Nil
      }

    @tailrec
    def rec(sentence: List[String], acc: List[Option[String]]): List[Option[String]] =
      sentence match {
        case x :: Nil => rec(Nil, acc ::: checksumSplit(x).map(emptyStringIsNull))
        case x :: xs  => rec(xs, acc ::: emptyStringIsNull(x) :: Nil)
        case Nil      => acc
      }

    def naiveList: Try[List[String]] = isValid match {
      case Success(sntnc) => Try(sntnc.split(',').toList)
      case Failure(e)     => Failure(e)
    }

    naiveList match {
      case Success(list) => Try(rec(list, List()))
      case Failure(e)    => Failure(e)
    }
  }

  def tag: Try[Option[String]] = this.asList match {
    case Success(list) => Try(list.head)
    case Failure(e)    => Failure(e)
  }

  def checksum: Try[Option[String]] = this.asList match {
    case Success(list) => Try(list.reverse.head)
    case Failure(e)    => Failure(e)
  }
}
