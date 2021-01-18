// package karger

import scala.io.Source

case class NodeItem(var n: String, var es: List[String])

object Main {

  val inputLines = Source.fromFile(path("test")).getLines().toList

  def path(x: String) = s"/home/matt/documents/coursera_algorithms/4/src/main/resources/$x.txt"

  def getNodeList(x: String) =
    Source.fromFile(path(x))
      .getLines().toList
      .map(_.split(" ").toList
        match { case (h :: t) => NodeItem(h,t)})

  def chooseNodes(nodeList: List[NodeItem]) = {
    var r = scala.util.Random
    var intersectPairs: List[(NodeItem,NodeItem)] = List()

    while (intersectPairs.length == 0) {
      val shuffledNodes = r.shuffle(nodeList)
      val pairedNodes = shuffledNodes.zip(shuffledNodes.tail)
      intersectPairs = pairedNodes.filter {
        case (ni1,ni2) =>
          ni1.es.contains(ni2.n) && ni2.es.contains(ni1.n)
      }
      // println(intersectPairs)
      // System.exit(0)
    }

    println(intersectPairs)
    val (ni1,ni2) = intersectPairs.head
    (ni1.n,ni2.n)
  }

  def mergeNodePair(nodeList: List[NodeItem]) = {
    val ni1 :: ni2 :: tail = nodeList
    val mergedName = ni1.n + "|" + ni2.n
    val mergedEdgeList = ni1.es.appendedAll(ni2.es).map {
      x => if (x == ni1.n || x == ni2.n) mergedName else x
    }.distinct.filter { _ != mergedName }
    NodeItem(mergedName, mergedEdgeList)
  }

  def karger(nl: List[NodeItem]) = {
    def go() = {
      var nodeList = nl
      // While there are more than two nodes left:
      // Reduce the edge and update the graph, deleting loops but not parallel edges
      // The make up of those two nodes are the two graphs, with the min cut being the edges between
      // Final answer is the lowest values after all the loop
      while (nodeList.length > 2) {
        // Choose an edge to reduce
        val chosenNodes = chooseNodes(nodeList)
        println(chosenNodes)
        val (chosen,rest) = nodeList.partition
          { case NodeItem(n,_) => n == chosenNodes._1 || n == chosenNodes._2 }
        val merged = mergeNodePair(chosen)
        val updatedRest = rest.map {
          case NodeItem(n,es) =>
            val updatedEdges = es.map
              { x => if (x == chosenNodes._1 || x == chosenNodes._2) merged.n else x }
            NodeItem(n, updatedEdges)
        }
        nodeList = updatedRest.appendedAll(List(merged))
        println(nodeList)
      }
      nodeList.flatMap(_.es).length
    }

    var lowest = 100000000
    for (i <- 0 to (nl.length)) {
      println(s"Run $i of ${nl.length}")
      val minCut = go()
      lowest = if (minCut < lowest) minCut else lowest
    }
    lowest
  }

  def main(args: Array[String]): Unit = {
    println("Karger Min Cuts Algorithm")
    println(inputLines.mkString("\n"))

    println("\nRun:\n")
    // val minCut = karger(getNodeList("test"))
    println(karger(getNodeList("input")))
  }
}
