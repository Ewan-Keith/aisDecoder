object convertAis {

  // convert a single AIS message character to Decimal Byte
  def charToDecimalByte(character: Option[Char]): Option[Int] =
    character match {
      case Some(char) => Some(char.toByte.toInt)
      case None       => None
    }
  
  // convert an 8 bit decimal (byte) to 6 bit decimal representation
  def eightBitToSixBit(eightBit: Int): Int = eightBit

  def messageToSixBit(message: String): List[Char] = {
    message.toList.map(char => char)
  }
  
  
}

object testing {

  def printList(args: TraversableOnce[_]): Unit = {
    args.foreach(println)
  }
  def test() = printList(convertAis.charToDecimalByte(Some('T')))
}