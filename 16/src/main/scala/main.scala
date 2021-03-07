package coursera

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Clause(negX: Boolean, x: Int, negY: Boolean, y: Int) {
  override def toString() = s"(${if (negX) "-" else ""}${x},${if (negY) "-" else ""}${y})"
}

object Input {
  def inputFile(x: String) = s"/home/matt/documents/coursera_algorithms/16/src/main/resources/${x}.txt"

  def parseInput(fileName: String): List[Clause] = {
    Source.fromFile(inputFile(fileName)).getLines.toList.tail
      .map { line =>
        line.split("\\s+").map(_.toInt) match {
          case Array(l,r) => Clause(l < 0, Math.abs(l), r < 0, Math.abs(r))
        }
      }
  }
}

object Reducer {

  // Also need to return the set values from eliminating the clause
  def reduce(clauses: List[Clause]): List[Clause] = {
    val setX = ArrayBuffer.fill(clauses.size)(false)
    val negX = ArrayBuffer.fill(clauses.size)(false)
    clauses.foreach { case Clause(nx, x, ny, y) =>
      if (nx) negX(x) = true else setX(x) = true
      if (ny) negX(y) = true else setX(y) = true
    }
    val fixed = for (i <- 0 to (clauses.size - 1))
                yield (i, setX(i) ^ negX(i)).filter{ match (_,s) => s }
    println(fixed)
    clauses
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("\n\nPapadimitriou's Algorithmn\n")
    val input = Input.parseInput("tc1")
    println(input)
    Reducer.reduce(input)
  }

}
