package dijkstra

import scala.collection.mutable.ArrayBuffer

/*
 * Need to implement
 * add
 * popMin
 * peekMin
 */
class Heap {
  val arr = ArrayBuffer[Int]()

  def isEmpty(): Boolean = arr.length == 0

  def peekMin(): Option[Int] = None

  def popMin(): Option[Int] = None

  def add(x: Int): Unit = {
    arr(0) = x
  }

  override def toString(): String = arr.toString
}
