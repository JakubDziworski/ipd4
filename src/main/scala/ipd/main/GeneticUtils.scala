package ipd.main

object GeneticUtils {

  def init(iterationNumber: Int, chromosomeNumber: Int, crossProbability: Double, mutationProbability: Double,
           reproductionProbability: Double): Unit = {
    var i = 1

    println("Pokolenie 1")
    var generation = initFirstGeneration(chromosomeNumber)
//    generation.foreach(println)
//    println

    var maxPoint = globalMaximum(generation)
    println("Najlepszy chromoson: " + maxPoint)
    println

    while(i < iterationNumber) {
      println("Pokolenie " + (i+1))
      generation = createNewGeneration(generation, crossProbability, mutationProbability, reproductionProbability)

//      generation.foreach(println)
//      println

      maxPoint = globalMaximum(generation)
      println("Najlepszy chromoson: " + maxPoint)
      println

      i += 1
    }
  }

  private def initFirstGeneration(number: Int): List[Chromosome] = {
    var generation = List[Chromosome]()

    for (i <- 1 to number) {
      val binaryValue = BinaryUtils.randomCodedBinary()
      val decimalValue = BinaryUtils.toDecimal(binaryValue)
      val functionValue = Settings.function(decimalValue)
      val chromosome = Chromosome(decimalValue, functionValue, binaryValue)
      generation = generation :+ chromosome
    }

    generation
  }

  private def globalMaximum(generation: List[Chromosome]): Chromosome = {
    generation.find(e => e.functionValue == generation.map(e => e.functionValue).max).get
  }

  private def createNewGeneration(generation: List[Chromosome], crossProbability: Double, mutationProbability: Double,
                                  reproductionProbability: Double): List[Chromosome] = {
    val reproductionChromosomes = getChromosomesToReproduction(generation, RandomUtils.randomNumbers(0, 100, generation.size))
    var newGeneration = List[Chromosome]()
    var i = 0

   //print("Operacje genetyczne: ")

    while(newGeneration.size < generation.size) {
      val geneticOperation = RandomUtils.randomNumber(0, 100)

      if (geneticOperation <= crossProbability && i < generation.size - 2) {
        //print("krzyzowanie, ")
        newGeneration = newGeneration ++ GeneticUtils.createChromosomes(reproductionChromosomes(i),
          reproductionChromosomes(i+1))
        i += 2
      } else if (geneticOperation > crossProbability && geneticOperation <= crossProbability + mutationProbability) {
        //print("mutacja, ")
        newGeneration = newGeneration :+ GeneticUtils.mutateChromosome(reproductionChromosomes(i))
        i += 1
      } else if (geneticOperation > crossProbability + mutationProbability
          && geneticOperation <= crossProbability + mutationProbability + reproductionProbability) {
       //print("kopiowanie, ")
        newGeneration = newGeneration :+ GeneticUtils.copyChromosome(reproductionChromosomes(i))
        i += 1
      }
    }

    println

    newGeneration
  }

  private def getChromosomesToReproduction(generation: List[Chromosome], numbers: List[Double]): List[Chromosome] = {
    val rouletteFields = RouletteUtils.getRouletteFields(generation)
    var bestChromosomes = List[Chromosome]()

    for (i <- numbers.indices) {
      for (j <- rouletteFields.indices) {
        if (numbers(i) >= rouletteFields(j).valueFrom && numbers(i) <= rouletteFields(j).valueTo) {
          bestChromosomes = bestChromosomes :+ rouletteFields(j).chromosome
        }
      }
    }

    bestChromosomes
  }

  private def copyChromosome(chromosome: Chromosome): Chromosome = {
    Chromosome(chromosome.decimalValue, chromosome.functionValue, chromosome.binaryValue)
  }

  private def mutateChromosome(chromosome: Chromosome): Chromosome = {
    var binaryValue = chromosome.binaryValue
    val charIndex = RandomUtils.randomIntNumber(binaryValue.length)
    var binaryChar = binaryValue(charIndex)

    if (binaryChar.equals('1')) {
      binaryChar = '0'
    } else {
      binaryChar = '1'
    }

    binaryValue = binaryValue.substring(0,charIndex) + binaryChar + binaryValue.substring(charIndex + 1)

    val decimalValue = BinaryUtils.toDecimal(binaryValue)
    val functionValue = Settings.function(decimalValue)

    Chromosome(decimalValue, functionValue, binaryValue)
  }

  private def createChromosomes(parent1: Chromosome, parent2: Chromosome): List[Chromosome] = {
    val bitsNumber = parent1.binaryValue.length
    val crossPoint = RandomUtils.randomNumber(0, bitsNumber-2).toInt

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
