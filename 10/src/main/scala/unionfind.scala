package coursera

import scala.collection.mutable.Map

private case class UFVal(x: Int, var leader: Option[UFVal])

/*
 * Supports lazy unions and path compression,
 * but not union by rank
 */
class UnionFind() {
  private var setC = 0
  private val collection = Map[Int,UFVal]()

  // Public methods
  def setCount() = setC

  def find(x: Int): Option[Int] = findUFVal(x).map(_.x)

  def merge(x: Int, y: Int): Unit = {
    val xLead = findUFVal(x)
    val yLead = findUFVal(y)
    if (xLead != None && yLead != None && yLead != xLead) {
      xLead.get.leader = yLead
      setC -= 1
    }
  }

  def makeSet(x: Int): Unit = {
    if (collection.contains(x)) return
    else {
      setC += 1
      collection.addOne((x, UFVal(x, None)))
    }
  }

  // Private methods

  private def findUFVal(x: Int): Option[UFVal] = {
    def go(ufval: UFVal): UFVal = {
      if (ufval.leader != None) {
        ufval.leader = Some(go(ufval.leader.get))
        ufval.leader.get
      } else ufval
    }
    collection.get(x).map { ufval =>
      go(ufval)
    }
  }

  def debug(): Unit = {
    for (k <- collection.keys) {
      println(collection.get(k).get)
    }
  }
}
