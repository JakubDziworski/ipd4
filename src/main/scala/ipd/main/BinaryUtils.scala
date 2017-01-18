package ipd.main

object BinaryUtils {

  private val BITS_NUMBER = bitsNumber()

  def toDecimal(codedBinary: String): Double = {
    Settings.MIN_FUNCTION_DOMAIN + toDecimalN(codedBinary) *
      (Settings.MAX_FUNCTION_DOMAIN - Settings.MIN_FUNCTION_DOMAIN) / (Math.pow(2, BITS_NUMBER) - 1)
  }

  def randomCodedBinary(): String = {
    var randomCodedBinary = ""

    for(i <- 0 until BinaryUtils.BITS_NUMBER) {
      randomCodedBinary = randomCodedBinary + RandomUtils.randomIntNumber(2)
    }

    randomCodedBinary
  }

  private def toDecimalN(binary: String): Int = Integer.parseInt(binary, 2)

  private def bitsNumber(): Int = {
    val a = (Settings.MAX_FUNCTION_DOMAIN - Settings.MIN_FUNCTION_DOMAIN) * Math.pow(10, Settings.DECIMAL_PLACES) + 1
    var bitsNumber = 1
    while(Math.pow(2, bitsNumber) < a) {
      bitsNumber += 1
    }

    bitsNumber
  }

}
