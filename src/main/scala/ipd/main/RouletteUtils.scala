package ipd.main

object RouletteUtils {

  def getRouletteFields(generation: List[Chromosome]): List[RouletteField] = {
    val min = generation.map(_.functionValue).min
    val max = generation.map(_.functionValue).max
    val sorted = generation.sortBy(_.functionValue)

    sorted.zip(sorted.tail).map { case (startChromosome,endChromosome) =>
      val from = normalize(min, max, startChromosome.functionValue)
      val to = normalize(min,max,endChromosome.functionValue)
      RouletteField(endChromosome,from,to)
    }
  }

  def normalize(min:Double,max:Double,value:Double) : Double = {
    val size = max - min
    (Math.abs(value - min)/size)*100.0
  }


}
