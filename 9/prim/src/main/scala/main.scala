package coursera

import scala.io.Source
import scala.collection.mutable.Map

case class Node(name: String, edges: List[Tuple2[String,Int]])

object File {
  def inputFile(fileName: String) =
    s"/home/matt/documents/coursera_algorithms/9/prim/src/main/resources/${fileName}.txt"

  def inputLines(fileName: String) =
    Source.fromFile(inputFile(fileName)).getLines.toList

  def parseEdgeList(in: List[String]): List[Tuple3[String,String,Int]] = {
    in.tail.map { x =>
      x.split("\\s+") match {
        case Array(n1,n2,w) => (n1,n2,w.toInt)
      }
    }
  }

  def toNodeList(edgeList: List[Tuple3[String,String,Int]]): Map[String,Node] = {
    val mm = Map[String,Node]()

    def updateMap(from: String, to: String, weight: Int) = {
      val fromMap = mm.get(from)
      val node = fromMap.map { n =>
        n.copy(edges=((to,weight) :: n.edges))
      }.getOrElse(Node(from, List((to,weight))))
      mm.addOne((from,node))
    }

    for (edge <- edgeList) {
      updateMap(edge._1,edge._2,edge._3)
      updateMap(edge._2,edge._1,edge._3)
    }
    mm
  }

  def parseFileToGraph(fileName: String) = {
    val in = inputLines(fileName)
    val el = parseEdgeList(in)
    toNodeList(el)
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("\n\nPrim's")
    val graph = File.parseFileToGraph("tc1")
    println(graph)
  }

}
