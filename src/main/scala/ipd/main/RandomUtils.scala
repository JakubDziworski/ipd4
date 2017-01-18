package ipd.main

import scala.util.Random

object RandomUtils {

  // 0 to maxExclusiveValue-1
  def randomIntNumber(maxExclusiveValue: Int): Int = {
    val r = new Random()
    r.nextInt(maxExclusiveValue)
  }

  def randomNumber(rangeMin: Double, rangeMax: Double): Double = {
    val r = new Random()
    val number = rangeMin + (rangeMax - rangeMin) * r.nextDouble()
    BigDecimal(number).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def shuffleValues(fromValue: Int, toValue: Int): List[Int] = {
    Random.shuffle(fromValue to toValue).toList
  }

  def randomNumbers(rangeMin: Double, rangeMax: Double, size: Int): List[Double] = {
    var randomNumbers = List[Double]()
    for (i <- 1 to size) {
      randomNumbers = randomNumbers :+ RandomUtils.randomNumber(rangeMin, rangeMax)
    }
    randomNumbers
  }




}
