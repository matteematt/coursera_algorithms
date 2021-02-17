package coursera

import scala.io.Source

object C1 {
  case class Edge(n1: Int, n2: Int, e: Int)

  def filePath(x: String) = s"/home/matt/documents/coursera_algorithms/10/src/main/resources/${x}.txt"

  def parseFileToEdgeList(file: String): List[Edge] = {
    Source.fromFile(filePath(file)).getLines.toList.tail.map { x =>
      x.split("\\s+") match { case Array(a,b,c) => Edge(a.toInt,b.toInt,c.toInt) }
    }
  }

  def run(file: String, k: Int = 4): Int = {
    val edgeList = parseFileToEdgeList(file).sortWith((x,y) => x.e.compareTo(y.e) < 0)
    val edgeArr = edgeList.toArray
    val nodeList = edgeList.map{case Edge(n1,n2,_) => List(n1,n2)}.flatten.distinct

    val uf = new UnionFind[Int]()
    nodeList.foreach(x => uf.makeSet(x))

    var i = 0
    var done = false
    while (!done) {
      val n1 = edgeArr(i).n1
      val n2 = edgeArr(i).n2
      if (uf.find(n1) != uf.find(n2)) {
        if (uf.setCount == k) {
          done = true
          i -= 1
        } else {
          uf.merge(n1,n2)
        }
      }
      i += 1
    }
    edgeArr(i).e
  }
}

object C2 {

  def closeNumbers(x: Int, bits: Int, dist: Int = 1): List[Int] = {
    (0 to (bits-1)).toList.map(b => (1 << b) ^ x).flatMap {
      n => if (dist == 1) List(n) else n :: closeNumbers(n, bits, dist-1)
    }.distinct
  }

  def bitsToInt(x: String): Int = {
    x.split("\\s+").map(_.toInt).fold(0)((t,b) => if (b==1) t*2+1 else t*2)
  }

  def parseFile(file: String): Tuple2[Int,List[Int]] = {
    Source.fromFile(C1.filePath(file)).getLines.toList match {
      case h :: t => {
        val bits = h.split("\\s+").tail.head.toInt
        val nums = t.map(bitsToInt(_))
        (bits,nums)
      }
    }
  }

  def run(file: String): Int = {
    val uf = new UnionFind[Int]()
    val (bits,nums) = parseFile(file)
    nums.foreach(uf.makeSet(_))
    uf.contents.foreach { x =>
      closeNumbers(x,bits,2).foreach(cn => uf.merge(x,cn))
    }
    uf.setCount
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("\n\nClustering")
    println(C1.run("clustering1"))

    println(C2.run("clustering2"))
  }

}
