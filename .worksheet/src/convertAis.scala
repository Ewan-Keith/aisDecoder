object convertAis {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(251); 

  // convert a single AIS message character to Decimal Byte
  def charToDecimalByte(character: Option[Char]): Option[Int] =
    character match {
      case Some(char) => Some(char.toByte.toInt)
      case None       => None
    };System.out.println("""charToDecimalByte: (character: Option[Char])Option[Int]""");$skip(126); 
  
  // convert an 8 bit decimal (byte) to 6 bit decimal representation
  def eightBitToSixBit(eightBit: Int): Int = eightBit;System.out.println("""eightBitToSixBit: (eightBit: Int)Int""");$skip(97); 

  def messageToSixBit(message: String): List[Char] = {
    message.toList.map(char => char)
  };System.out.println("""messageToSixBit: (message: String)List[Char]""")}
  
  
}

object testing {

  def printList(args: TraversableOnce[_]): Unit = {
    args.foreach(println)
  }
  def test() = printList(convertAis.charToDecimalByte(Some('T')))
}
