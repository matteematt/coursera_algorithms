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
      val comb = HuffNode(
        larger.symbol + smaller.symbol,
        smaller.weight + larger.weight,
        Some(larger), Some(smaller))
      heap.add(comb)
    }

    val rootNode = heap.popMin().get
    println(rootNode)

    (minDepth(rootNode),maxDepth(rootNode))
  }

  def main(args: Array[String]): Unit = {
    println("\n\nHuffman Coding:")
    // println(run("t2"))
    println(run("input"))
  }
}
