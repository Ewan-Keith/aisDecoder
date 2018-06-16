object convertAistemp {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(255); 

  // convert a single AIS message character to Decimal Byte
  def charToDecimalByte(character: Option[Char]): Option[Int] =
    character match {
      case Some(char) => Some(char.toByte.toInt)
      case None       => None
    };System.out.println("""charToDecimalByte: (character: Option[Char])Option[Int]""");$skip(257); 


  // convert an 8 bit decimal (byte) to 6 bit decimal representation
  def eightBitToSixBit(eightBit: Option[Int]): Option[Int] =
    eightBit match {
      case Some(eightBit) => eightToSixBitCalculation(eightBit)
      case None           => None
    };System.out.println("""eightBitToSixBit: (eightBit: Option[Int])Option[Int]""");$skip(509); 

  // carries out the numerical 8 bit to 6 bit decimal conversion
  private def eightToSixBitCalculation(eightBit: Int): Option[Int] = {

    // subtract bit difference (48 = 6 * 8)
    val eightBitMinus = eightBit - 48

    // subtract 8 if bit value remains too high for 6 bit
    val sixBit = if (eightBitMinus < 40)
      eightBitMinus
    else
      eightBitMinus - 8

    // Catch value if outside valid range, else return
    if (sixBit <= 63 && sixBit >= 1)
      Some(sixBit)
    else
      None
  };System.out.println("""eightToSixBitCalculation: (eightBit: Int)Option[Int]""");$skip(108); 
  
  
  //

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
