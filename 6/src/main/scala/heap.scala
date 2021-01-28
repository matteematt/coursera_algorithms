package dijkstra

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import math.Ordering

/*
 * Need to implement
 * add
 * popMin
 * peekMin
 */
class Heap[O](implicit order: Ordering[O]) {
  private val arr = ArrayBuffer[O]()

  // Public Methods
  def isEmpty(): Boolean = arr.length == 0

  def peekMin(): Option[O] = {
    if (arr.length == 0) None
    else Some(arr(0))
  }

  def popMin(): Option[O] = arr.length match {
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

  def add(x: O): Unit = {
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
    if (order.lteq(arr(parentIndex),arr(index))) return
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

    if (lChild < arr.length && order.lt(arr(lChild),arr(smallest))) {
      smallest = lChild
    }

    if (rChild < arr.length && order.lt(arr(rChild),arr(smallest))) {
      smallest = rChild
    }

    if (smallest != index) {
      swap(index,smallest)
      bubbleDown(smallest)
    }
  }
}
