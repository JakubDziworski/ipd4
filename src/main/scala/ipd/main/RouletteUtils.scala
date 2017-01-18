package ipd.main

object RouletteUtils {

  def getRouletteFields(generation: List[Chromosome]): List[RouletteField] = {
    var normalizedFunctionValues = List[Double]()
    var rouletteFields = List[RouletteField]()
    var rouletteSum = 0.0
    var rouletteValueTo = 100.0
    val minFunctionValue = generation.map(e => e.functionValue).min

    //normalizacja wartosci funkcji
    if (minFunctionValue < 0) {
      normalizedFunctionValues = generation.map(e => e.functionValue + Math.abs(minFunctionValue))
    } else {
      normalizedFunctionValues = generation.map(e => e.functionValue)
    }
    //obliczenie lacznej wartosci elementow
    for (i <- normalizedFunctionValues.indices) {
      rouletteSum += normalizedFunctionValues(i)
    }

    val rouletteFromValues = normalizedFunctionValues.map(e => e / rouletteSum * 100)

    //wypelnianie ruletki
    for (i <- generation.size-1 to 0 by -1) {
      rouletteFields = RouletteField(generation(i), rouletteValueTo - rouletteFromValues(i),
        rouletteValueTo) :: rouletteFields
      rouletteValueTo -= rouletteFromValues(i)
    }

    rouletteFields
  }

}
