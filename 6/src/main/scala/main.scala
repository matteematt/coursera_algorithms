package dijkstra

import scala.io.Source

case class Node(n: Int, paths: List[Tuple2[Int,Int]])

object Main {
  def filePath(x: String) = s"/home/matt/documents/coursera_algorithms/6/${x}.txt"

  def readInput(file: String): List[String] =
    Source.fromFile(filePath(file))
    .getLines().toList

  def parseTuple(x: String): Tuple2[Int,Int] = x.split(",") match {
    case Array(a,b,_*) => (a.toInt,b.toInt)
    case _ => throw new Error(s"Unable to parseTuple '${x}'")
  }


  def parseGraph(xs: List[String]): List[Node] = {
    xs.map(_.split("\\s+").toList match {
      case h :: t => Node(h.toInt, t.map(x => parseTuple(x)))
      case _ => throw new Error(s"Unable to parse input line")
    })
  }

  def main(args: Array[String]): Unit = {
    println("\n\nDijkstra's")

    val graph = parseGraph(readInput("input"))
    println(graph)

    val heap = new Heap[Int]()
  }
}
