package com.ewankeith.aisdecoder.payload

import scala.util.{ Try, Success, Failure }

case class PayloadByte(eightBitDecimal: Byte) {

  // six bit decimal representation, catches any invalid 8 bit inputs
  def asSixBitDecimal: Try[Int] = {
    if (eightBitDecimal >= 48 && eightBitDecimal <= 87)
      Try(eightBitDecimal - 48)
    else if (eightBitDecimal >= 96 && eightBitDecimal <= 119)
      Try(eightBitDecimal - 56)
    else
      Failure(new Exception(
        s"Invalid payload byte in PayloadBit conversion: $eightBitDecimal"))
  }

  // sixBitDecimal as a six long bit stream, as a List[Int].
  def asBitStream: Try[List[Int]] =
    asSixBitDecimal match {
      case Success(sbd) => Try(
        String.format("%6s", sbd.toBinaryString)
          .replace(' ', '0')
          .toList
          .map(_.asDigit))
      case Failure(msg) => Failure(msg)
    }

  // to print as a single string
  override def toString =
    this.asBitStream match {
      case Success(list) => list.mkString
      case Failure(msg)  => Failure(msg).toString
    }

}