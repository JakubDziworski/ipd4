package ipd.main

import scala.util.Random

object RandomUtils {

  // 0 to maxExclusiveValue-1
  def randomIntNumber(maxExclusiveValue: Int): Int = {
    Random.nextInt(maxExclusiveValue)
  }

  def randomNumber(rangeMin: Double, rangeMax: Double): Double = {
    val number = rangeMin + (rangeMax - rangeMin) * Random.nextDouble()
    BigDecimal(number).setScale(10, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def shuffleValues(fromValue: Int, toValue: Int): List[Int] = {
    Random.shuffle(fromValue to toValue).toList
  }

  def randomNumbers(rangeMin: Double, rangeMax: Double, size: Int): List[Double] = {
    List.fill(size)(RandomUtils.randomNumber(rangeMin, rangeMax))
  }

}
