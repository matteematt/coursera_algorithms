package karger

import scala.io.Source

case class NodeItem(var n: String, var es: List[String])

object Main {

  def path(x: String) = s"/home/matt/documents/coursera_algorithms/4/src/main/resources/$x.txt"

  def getNodeList(x: String) =
    Source.fromFile(path(x))
      .getLines().toList
      .map(_.split("\\s+").toList
        match { case (h :: t) => NodeItem(h,t)})

  def chooseNodes(nodeList: List[NodeItem]) = {
    var r = scala.util.Random
    // Randomly pick a node
    val ni1 = r.shuffle(nodeList).head
    // Randomly pick an edge from that node
    val edge = r.shuffle(ni1.es).head
    (ni1.n,edge)
  }

  def mergeNodePair(nodeList: List[NodeItem]) = {
    val ni1 :: ni2 :: tail = nodeList
    val mergedName = ni1.n + "|" + ni2.n
    val mergedEdgeList = ni1.es.appendedAll(ni2.es).map {
      x => if (x == ni1.n || x == ni2.n) mergedName else x
    }.filter { _ != mergedName }
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
        // Partition out the two chosen nodes
        val (chosen,rest) = nodeList.partition
          { case NodeItem(n,_) => n == chosenNodes._1 || n == chosenNodes._2 }
        // Merge the two selected nodes
        val merged = mergeNodePair(chosen)
        // Update the rest of the nodes with the merged nodes info
        val updatedRest = rest.map {
          case NodeItem(n,es) =>
            val updatedEdges = es.map
              { x => if (x == chosenNodes._1 || x == chosenNodes._2) merged.n else x }
            NodeItem(n, updatedEdges)
        }
        // Place the merged node into the node list
        nodeList = updatedRest.appendedAll(List(merged))
      }
      // The min cut of this iteration is the number of edges from one node to another
      nodeList.head.es.length
    }

    // Run n times, keeping the lowest score
    var lowest = 100000000
    for (i <- 0 to (nl.length)) {
      // println(s"Run $i of ${nl.length}")
      val minCut = go()
      lowest = if (minCut < lowest) minCut else lowest
    }
    lowest
  }

  def main(args: Array[String]): Unit = {
    println("Karger Min Cuts Algorithm")
    println("\nRun:\n")
    // println(karger(getNodeList("test")))
    println(karger(getNodeList("input")))
  }
}
