package coursera

import scala.collection.mutable.Map

private case class UFVal[T](x: T, var leader: Option[UFVal[T]])

/*
 * Supports lazy unions and path compression,
 * but not union by rank
 */
class UnionFind[T]() {
  private var setC = 0
  private val collection = Map[T,UFVal[T]]()

  // Public methods
  def setCount() = setC

  def find(x: T): Option[T] = findUFVal(x).map(_.x)

  def merge(x: T, y: T): Unit = {
    val xLead = findUFVal(x)
    val yLead = findUFVal(y)
    if (xLead != None && yLead != None && yLead != xLead) {
      xLead.get.leader = yLead
      setC -= 1
    }
  }

  def makeSet(x: T): Unit = {
    if (collection.contains(x)) return
    else {
      setC += 1
      collection.addOne((x, UFVal(x, None)))
    }
  }

  // Private methods

  private def findUFVal(x: T): Option[UFVal[T]] = {
    def go(ufval: UFVal[T]): UFVal[T] = {
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
