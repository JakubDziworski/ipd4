package ipd.main

case class Chromosome(decimalValue: Double, functionValue: Double, binaryValue: String) {

  override def toString: String = "x: " + decimalValue + ", y: " + functionValue + ", b: " + binaryValue

}