package ipd.main

object BinaryUtils {

  private val BITS_NUMBER = bitsNumber()

  def toDecimal(codedBinary: String): Double = {
    Settings.MIN_FUNCTION_DOMAIN + toDecimalN(codedBinary) *
      (Settings.MAX_FUNCTION_DOMAIN - Settings.MIN_FUNCTION_DOMAIN) / (Math.pow(2, BITS_NUMBER) - 1)
  }

  def randomCodedBinary(): String = {
    List.fill(BinaryUtils.BITS_NUMBER)(RandomUtils.randomIntNumber(2)).mkString("")
  }

  private def toDecimalN(binary: String): Int = Integer.parseInt(binary, 2)

  private def bitsNumber(): Int = {
    val possibilities = (Settings.MAX_FUNCTION_DOMAIN - Settings.MIN_FUNCTION_DOMAIN) * Math.pow(10, Settings.DECIMAL_PLACES) + 1
    Stream.from(1).takeWhile(bits => Math.pow(2, bits) <= possibilities).last
  }

}
