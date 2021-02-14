package coursera

import scala.io.Source
import scala.collection.mutable.Map
import scala.annotation.tailrec

case class Node(n: Int, paths: List[Tuple2[Int,Int]], var score: Option[Int])

// Where score is dijkstra's greedy score
case class Path(tail: Int, head: Int, score: Int) extends Ordered[Path] {
  def compare(that: Path) = this.score - that.score
}

object Graph {
  private def filePath(x: String) = s"/home/matt/documents/coursera_algorithms/6/${x}.txt"

  private def readInput(file: String): List[String] =
    Source.fromFile(filePath(file))
    .getLines().toList

  private def parseTuple(x: String): Tuple2[Int,Int] = x.split(",") match {
    case Array(a,b,_*) => (a.toInt,b.toInt)
    case _ => throw new Error(s"Unable to parseTuple '${x}'")
  }


  private def parseGraph(xs: List[String]): List[Node] = {
    xs.map(_.split("\\s+").toList match {
      case h :: t => Node(h.toInt, t.map(x => parseTuple(x)), None)
      case _ => throw new Error(s"Unable to parse input line")
    })
  }

  // Public methods
  def parseGraphFromFile(file: String): Map[Int,Node] = {
    val listGraph = parseGraph(readInput(file))
    val graph = Map[Int,Node]()
    for (node <- listGraph) {
      graph.addOne(node.n, node)
    }
    graph
  }
}

object Search {
  val unconnected = 1000000

  def dijkstra(graph: Map[Int,Node], from: Int, to: Int): Int = {
    val heap = new Heap[Path]()
    val visited = Map[Int,Boolean]()
    go(graph,heap,visited,0,from,to)
  }

  @tailrec
  private def go(
    graph: Map[Int,Node],
    heap: Heap[Path],
    visited: Map[Int,Boolean],
    pathScore: Int,
    from: Int,
    to: Int): Int = {
    // Get the new paths at the frontier
    val node = graph.get(from).getOrElse(throw new Error(s"Unable to find node $from in graph"))
    visited.addOne(node.n,true)
    node.score = Some(pathScore)
    heap.cullBy(x => x.head == node.n)

    // Get all the new paths from the increased frontier
    val paths = node.paths.map { case (head,length) =>
      Path(from,head,(graph.get(from).get.score.get + length))
    }.filter(x => !visited.contains(x.head))

    // Update the heap with all thew new paths, but only if the new one is a smaller size
    // TODO: could be made more efficient by not ripping the old one out if its better (smaller)
    paths.foreach { x =>
      val oldVal = heap.deleteArbitrary(y => x.head == y.head)
      if (oldVal.isDefined && oldVal.get.score < x.score) heap.add(oldVal.get)
      else heap.add(x)
    }
    // If bestPath = None then no path and we need to return unconnected
    // TODO: Need to implement the throw and catch properly for unconnected graphs
    val bestPath = heap.popMin().getOrElse(throw new Error(s"Unable to find a next path"))

    if (bestPath.head == to) bestPath.score
    else go(graph, heap, visited, bestPath.score, bestPath.head, to)
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    val graph = Graph.parseGraphFromFile("input")
    val toList = List(7,37,59,82,99,115,133,165,188,197)
    val outList = toList.map(x => Search.dijkstra(graph, 1, x))
    println(outList.mkString(","))
  }
}
