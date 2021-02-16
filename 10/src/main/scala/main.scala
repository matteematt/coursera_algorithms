package coursera

import scala.io.Source

object C1 {
  val k = 4

  case class Edge(n1: Int, n2: Int, e: Int)

  def filePath(x: String) = s"/home/matt/documents/coursera_algorithms/10/src/main/resources/${x}.txt"

  def parseFileToEdgeList(file: String): List[Edge] = {
    Source.fromFile(filePath(file)).getLines.toList.tail.map { x =>
      x.split("\\s+") match { case Array(a,b,c) => Edge(a.toInt,b.toInt,c.toInt) }
    }
  }

  def run(file: String): Int = {

    val edgeList = parseFileToEdgeList(file).sortWith((x,y) => x.e.compareTo(y.e) < 0)
    val edgeArr = edgeList.toArray
    val nodeList = edgeList.map{case Edge(n1,n2,_) => List(n1,n2)}.flatten.distinct

    val uf = new UnionFind[Int]()
    nodeList.foreach(x => uf.makeSet(x))

    var i = 0
    while (uf.setCount >= 4) {
      val n1 = edgeArr(i).n1
      val n2 = edgeArr(i).n2
      if (uf.find(n1) != uf.find(n2)) {
        uf.merge(n1,n2)
      }
      i += 1
    }
    edgeArr(i).e
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("\n\nClustering")
    println(C1.run("c1t1"))
    println(C1.run("clustering1"))
  }

}
