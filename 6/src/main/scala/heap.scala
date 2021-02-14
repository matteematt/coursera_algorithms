package coursera

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import math.Ordering

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

  // Need to try and find a way to delete in less time
  def deleteArbitrary(fn: O => Boolean): Option[O] =
    findIndex(fn).map(i => deleteByIndex(i))

  // Overwrites the first value satisfying fn (bubbling to keep a valid heap)
  // or adds the value if it doesn't exist
  def addUpdateArbitrary(fn: O => Boolean, next: O): Unit = {
    findIndex(fn).map(i => deleteByIndex(i))
    add(next)
  }

  def cullBy(fn: O => Boolean): Unit = {
    val indices = for {
      i <- 0 to (arr.length-1)
      if fn(arr(i)) } yield i
    // Need to shift values as values before them are removed
    val offsetIndices = (0 to (indices.length-1)).zip(indices)
      .map{ case (o,i) => i-o }
    offsetIndices.foreach(i => deleteByIndex(i))
  }

  override def toString(): String = arr.toString

  // Private Methods

  // it is assumed that its called in bounds as this isn't a public interface
  private def deleteByIndex(i: Int): O = {
    val deleted = arr(i)
    arr(i) = arr(arr.length-1)
    arr.trimEnd(1)
    bubbleDown(i)
    deleted
  }

  private def findIndex(fn: O => Boolean): Option[Int] = {
    for (index <- 0 to (arr.length-1)) {
      if (fn(arr(index))) {
        return Some(index)
      }
    }
    None
  }

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
