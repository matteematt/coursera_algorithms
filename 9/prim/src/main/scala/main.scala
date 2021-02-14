package coursera

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.annotation.tailrec

case class Node(name: String, edges: List[Tuple2[String,Int]])

case class Edge(from: String, to: String, weight: Int) extends Ordered[Edge] {
  def compare(that: Edge) = this.weight - that.weight
}

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

object Prim {
  def mst(graph: Map[String,Node]): Int = {
    // Randomly choose one vertex
    // Add vertex length to count, add next vertices to heap
    // choose the shortest node to a node we haven't seen
    // keep going until we have seen all

    // Init
    var seen = Set[String]()
    var heap = new Heap[Edge]()

    val r = scala.util.Random
    val nodes = graph.keySet.toVector
    val fst = nodes(r.nextInt(nodes.size))

    go(fst,seen,heap,graph,0)
  }

  @tailrec
  private def go(
    from: String,
    seen: Set[String],
    heap: Heap[Edge],
    graph: Map[String,Node],
    mstLen: Int
  ): Int = {
    val node = graph.get(from).get
    seen.addOne(from)

    node.edges.filter(x => !seen.contains(x._1)).map {
      case (to: String, weight: Int) => Edge(from,to,weight)
    }.foreach(x => heap.add(x))

    var chosenEdge = heap.popMin().get
    while (seen.contains(chosenEdge.to) && !heap.isEmpty()) {
      chosenEdge = heap.popMin().get
    }

    if (heap.isEmpty) mstLen
    else {
      go(chosenEdge.to,seen,heap,graph,mstLen + chosenEdge.weight)
    }
  }
}

object Main {
  // -3612829

  def main(args: Array[String]): Unit = {
    println("\n\nPrim's")

    val graph = File.parseFileToGraph("input")
    println(Prim.mst(graph))
  }

}
