package ipd.main

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object GeneticUtils {

  def init(iterationNumber: Int, populationSize: Int, crossProbability: Double, mutationProbability: Double,
           reproductionProbability: Double): Unit = {
    Stream.iterate(initFirstGeneration(populationSize), iterationNumber) { prevGen =>
      createNewGeneration(prevGen, crossProbability, mutationProbability, reproductionProbability)
    }.zipWithIndex.foreach { case (generation, index) =>
      val maxPoint = getMaximumChromosome(generation)
      println(s"Pokolenie ${index + 1}")
      println(s"Najlepszy chromoson: $maxPoint\n")
    }
  }

  private def initFirstGeneration(populationSize: Int): List[Chromosome] = {
    List.fill(populationSize) {
      val binaryValue = BinaryUtils.randomCodedBinary()
      val decimalValue = BinaryUtils.toDecimal(binaryValue)
      val functionValue = Settings.function(decimalValue)
      Chromosome(decimalValue, functionValue, binaryValue)
    }
  }

  private def getMaximumChromosome(generation: List[Chromosome]): Chromosome = {
    generation.max(Ordering[Double].on[Chromosome](_.functionValue))
  }


  private def createNewGeneration(generation: List[Chromosome], crossProbability: Double, mutationProbability: Double,
                                  reproductionProbability: Double): List[Chromosome] = {
    val reproductionChromosomes = getChromosomesToReproduction(generation, RandomUtils.randomNumbers(0, 100, generation.size))
    var newGeneration = ArrayBuffer[Chromosome]()
    println(reproductionChromosomes.size)
    while (newGeneration.size < generation.size) {
      val i = newGeneration.size
      val geneticOperation = RandomUtils.randomNumber(0, crossProbability+mutationProbability+reproductionProbability)
      if (geneticOperation <= crossProbability && i < generation.size - 2) {
        newGeneration ++= createChromosomes(reproductionChromosomes(i), reproductionChromosomes(i + 1))
      } else if (geneticOperation <= crossProbability + mutationProbability) {
        newGeneration += mutateChromosome(reproductionChromosomes(i))
      } else {
        newGeneration += reproductionChromosomes(i)
      }
    }
    newGeneration.toList
  }

  private def getChromosomesToReproduction(generation: List[Chromosome], numbers: List[Double]): List[Chromosome] = {
    val rouletteFields = RouletteUtils.getRouletteFields(generation)
    println(rouletteFields.size)
    for {
      number <- numbers
      field <- rouletteFields if(number >= field.valueFrom && number <= field.valueTo)
    } yield field.chromosome
  }

  private def mutateChromosome(chromosome: Chromosome): Chromosome = {
    val binaryValue = chromosome.binaryValue
    val charIndex = RandomUtils.randomIntNumber(binaryValue.length)
    val binaryChar = if (binaryValue(charIndex) == '0') '1' else '0'

    val newBinaryValue = binaryValue.substring(0, charIndex) + binaryChar + binaryValue.substring(charIndex + 1)
    val newDecimalValue = BinaryUtils.toDecimal(binaryValue)
    val newFunctionValue = Settings.function(newDecimalValue)
    Chromosome(newDecimalValue, newFunctionValue, newBinaryValue)
  }

  private def createChromosomes(parent1: Chromosome, parent2: Chromosome): List[Chromosome] = {
    val bitsNumber = parent1.binaryValue.length
    val crossPoint = RandomUtils.randomNumber(0, bitsNumber - 2).toInt

    val binaryValue1 = parent1.binaryValue.substring(0, crossPoint) + parent2.binaryValue.substring(crossPoint,
      bitsNumber)
    val decimalValue1 = BinaryUtils.toDecimal(binaryValue1)
    val functionValue1 = Settings.function(decimalValue1)

    val binaryValue2 = parent2.binaryValue.substring(0, crossPoint) + parent1.binaryValue.substring(crossPoint,
      bitsNumber)
    val decimalValue2 = BinaryUtils.toDecimal(binaryValue2)
    val functionValue2 = Settings.function(decimalValue2)

    List[Chromosome](Chromosome(decimalValue1, functionValue1, binaryValue1),
      Chromosome(decimalValue2, functionValue2, binaryValue2))
  }

}
