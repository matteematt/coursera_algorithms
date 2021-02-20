package coursera

import scala.io.Source

case class HuffNode(
  symbol: String,
  weight: Int,
  left: Option[HuffNode],
  right: Option[HuffNode]
) extends Ordered[HuffNode] {
  def compare(that: HuffNode) = this.weight.compare(that.weight)
  def leaf() = left.isEmpty && right.isEmpty
}

object Main {

  def filename(x: String) =
    s"/home/matt/documents/coursera_algorithms/11/huffman/src/main/resources/${x}.txt"

  def parseHuffNodes(file: String): List[HuffNode] = {
    Source.fromFile(filename(file)).getLines.toList.tail.zipWithIndex.map {
      case (w,i) => HuffNode(s"$i", w.toInt, None, None)
    }
  }

  def minDepth(root: HuffNode): Int = {
    def go(node: HuffNode, depth: Int): Int = {
      if (node.left.flatMap(x => node.right.map(y => x.leaf || y.leaf)) == Some(true)) {
        depth + 1
      } else {
        go(node.left.get, depth + 1) min go(node.right.get, depth + 1)
      }
    }
    go(root,0)
  }

  def maxDepth(root: HuffNode): Int = {
    def go(node: HuffNode, depth: Int): Int = {
      if (node.leaf) depth
      else {
        val l = node.left.map(x => go(x,depth + 1)).getOrElse(-1)
        val r = node.right.map(x => go(x,depth + 1)).getOrElse(-1)
        l max r
      }
    }
    go(root,0)
  }

  def symbolSet(root: HuffNode): List[Tuple2[String,String]] = {
    def go(node: HuffNode, seq: String): List[Tuple2[String,String]] = {
      if (node.leaf) (s"${node.weight}",seq) :: Nil
      else {
        val l = node.left.map(x => go(x,seq + "0"))
        val r = node.right.map(x => go(x,seq + "1"))
        List(l,r).filter(_.isDefined).flatMap(_.get)
      }
    }
    go(root,"")
  }

  def run(file: String): Tuple2[Int,Int] = {
    val heap = new Heap[HuffNode]()
    parseHuffNodes(file).foreach(x => heap.add(x))
    println(heap)

    // Get the two smallest nodes
    // Create a new node with the bigger on the LHS and the smallest on the RHS
    // their weights and symbols combined
    // add them back to the heap
    while (heap.length > 1) {
      val smaller = heap.popMin().get
      val larger = heap.popMin().get
      println(s"Combining ${smaller.symbol} and ${larger.symbol}" )
      val comb = HuffNode(
        larger.symbol + smaller.symbol,
        smaller.weight + larger.weight,
        Some(smaller), Some(larger))
      heap.add(comb)
    }

    val rootNode = heap.popMin().get
    println(rootNode)
    println(symbolSet(rootNode).mkString("\n"))

    (maxDepth(rootNode),minDepth(rootNode))
  }

  def main(args: Array[String]): Unit = {
    println("\n\nHuffman Coding:")
    println(run("t3"))
    println(run("bitesize"))
    println(run("input"))
  }

