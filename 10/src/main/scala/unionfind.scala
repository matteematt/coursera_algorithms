package coursera

import scala.collection.mutable.Map

private case class UFVal(x: Int, var leader: Option[UFVal])

/*
 * Need to implement:
 * makeSet
 * find
 * merge
 * setCount
 * Make for int first and then generic
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
      xLead.get.leader = Some(yLead.get)
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
    collection.get(x).map { ufval =>
      var parent = ufval
      while (parent.leader != None) {
        parent = parent.leader.get
      }
      parent
    }
  }
}
