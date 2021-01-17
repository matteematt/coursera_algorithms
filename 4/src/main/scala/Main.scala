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
    }

    println(intersectPairs)
    val (ni1,ni2) = intersectPairs.head
    (ni1.n,ni2.n)
  }

  def karger(nodeList: List[NodeItem]) = {
    // Loop n times

    // While there are more than two nodes left:

    // Choose an edge to reduce
    val chosenNodes = chooseNodes(nodeList)
    println(chosenNodes)
    // Reduce the edge and update the graph, deleting loops but not parallel edges
    // The make up of those two nodes are the two graphs, with the min cut being the edges between
    // Final answer is the lowest values after all the loop
  }

  def main(args: Array[String]): Unit = {
    println("Karger Min Cuts Algorithm")
    println(inputLines.mkString("\n"))

    println("\nRun:\n")
    karger(getNodeList("test"))
  }
}
