package dijkstra

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

/*
 * Need to implement
 * add
 * popMin
 * peekMin
 */
class Heap {
  val arr = ArrayBuffer[Int]()

  // Public Methods
  def isEmpty(): Boolean = arr.length == 0

  def peekMin(): Option[Int] = {
    if (arr.length == 0) None
    else {
      Some(arr(0))
    }
  }

  def popMin(): Option[Int] = {
    if (arr.length == 0) None
    else {
      val min = arr(0)
      arr -= min
      Some(min)
    }
  }

  def add(x: Int): Unit = {
    arr.append(x)
    bubbleUp(arr.length-1)
  }

  override def toString(): String = arr.toString

  // Private Methods
  private def swap(li: Int, ri: Int): Unit = {
    val tmp = arr(li)
    arr(li) = arr(ri)
    arr(ri) = tmp
  }

  @tailrec
  private def bubbleUp(index: Int): Unit = {
    val parentIndex = ((index-1)/2).floor.toInt
    if (arr(parentIndex) <= arr(index)) return
    else {
      swap(parentIndex, index)
      bubbleUp(parentIndex)
    }
  }
}
