package ipd.main

object Settings {

  val DECIMAL_PLACES = 3

  val MIN_FUNCTION_DOMAIN = 0.5

  val MAX_FUNCTION_DOMAIN = 2.5

  def function(x: Double): Double = {
    (Math.exp(x) * Math.sin(10 * Math.PI * x) + 1) / x
  }

}
