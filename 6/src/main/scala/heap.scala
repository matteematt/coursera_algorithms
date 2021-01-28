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
  private val arr = ArrayBuffer[Int]()

  // Public Methods
  def isEmpty(): Boolean = arr.length == 0

  def peekMin(): Option[Int] = {
    if (arr.length == 0) None
    else Some(arr(0))
  }

  def popMin(): Option[Int] = arr.length match {
    case 0 => None
    case 1 => {
      val min = arr(0)
      arr -= min
      Some(min)
    }
    case _ => {
      val min = arr(0)
      arr(0) = arr(arr.length-1)
      arr.trimEnd(1)
      bubbleDown(0)
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

  @tailrec
  private def bubbleDown(index: Int): Unit = {
    val lChild = 2 * index+1
    val rChild = 2 * index+2
    var smallest = index

    if (lChild < arr.length && arr(lChild) < arr(smallest)) {
      smallest = lChild
    }

    if (rChild < arr.length && arr(rChild) < arr(smallest)) {
      smallest = rChild
    }

    if (smallest != index) {
      swap(index,smallest)
      bubbleDown(smallest)
    }
  }
}
