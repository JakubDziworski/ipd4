package ipd.main

object Main {
  def main(args: Array[String]): Unit = {
    val iterationNumber = 10000
    val generationNumber = 50
    val crossProbability = 0.5
    val mutationProbability = 0.25
    val copyProbability = 0.25
    GeneticUtils.init(iterationNumber, generationNumber, crossProbability, mutationProbability, copyProbability)
  }
}
