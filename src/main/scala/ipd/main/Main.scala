package ipd.main

object Main {
  def main(args: Array[String]): Unit = {
    print("Podaj liczbe iteracji: ")
    val iterationNumber = scala.io.StdIn.readLine().replace(",", ".").toInt

    print("Podaj liczebnosc pokolenia: ")
    val generationNumber = scala.io.StdIn.readLine().replace(",", ".").toInt

    print("Podaj prawdopodobienstwo krzyzowania: ")
    val crossProbability = scala.io.StdIn.readLine().replace(",", ".").toDouble

    print("Podaj prawdopodobienstwo mutacji: ")
    val mutationProbability = scala.io.StdIn.readLine().replace(",", ".").toDouble

    print("Podaj prawdopodobienstwo kopiowania: ")
    val copyProbability = scala.io.StdIn.readLine().replace(",", ".").toDouble

    println

    GeneticUtils.init(iterationNumber, generationNumber, crossProbability, mutationProbability, copyProbability)
  }

}
