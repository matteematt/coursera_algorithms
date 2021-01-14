package quicksort

import scala.io.Source

object Main {

  val filePath = "/home/matt/documents/coursera_algorithms/3/src/main/resources/input.txt"

  def main(args: Array[String]): Unit = {
    println("Hello World")
    val input = Source.fromFile(filePath).getLines
    println(input.max)
  }
}
