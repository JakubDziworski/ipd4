package ipd.main

object PseudoBinaryUtils {

  private val PARSE_SYSTEM = 2
  private val PSEUDO_BINARY_FRACTION_LENGTH = 10
  private val PSEUDO_BINARY_WHOLE_LENGTH = 2
  private val PSEUDO_BINARY_FIRST_WHOLE_INDEX = 0
  private val PSEUDO_BINARY_LAST_WHOLE_INDEX = 2
  private val PSEUDO_BINARY_FRACTION_PART = 1

  def toDecimal(binary: String): Double = {
    val whole = Integer.parseInt(binary.substring(PSEUDO_BINARY_FIRST_WHOLE_INDEX,
      PSEUDO_BINARY_LAST_WHOLE_INDEX), PARSE_SYSTEM)
    val fraction = Integer.parseInt(binary.substring(PSEUDO_BINARY_LAST_WHOLE_INDEX, binary.length), PARSE_SYSTEM)
    (whole.toString + "." + fraction.toString).toDouble
  }

  def toPseudoBinary(number: Double): String = {
    val whole = toPseudoBinary(number.toInt, PSEUDO_BINARY_WHOLE_LENGTH)
    val fraction = toPseudoBinary(decimalFraction(number), PSEUDO_BINARY_FRACTION_LENGTH)
    whole + fraction
  }

  private def toPseudoBinary(number: Int, digits: Int) =
    String.format("%" + digits + "s", number.toBinaryString).replace(' ', '0')

  private def decimalFraction(number: Double) = number.toString.split("\\.")(PSEUDO_BINARY_FRACTION_PART).toInt
}
