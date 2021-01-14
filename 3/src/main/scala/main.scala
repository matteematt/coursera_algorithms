package quicksort

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {

  val filePath = "/home/matt/documents/coursera_algorithms/3/src/main/resources/input.txt"
  val input: List[Int] = Source.fromFile(filePath).getLines.toList.map(_.toInt)

  def takeFirst(x: ArrayBuffer[Int]): Int = 0

  def swapIndices(x: ArrayBuffer[Int], l: Int, r: Int): Unit = {
    val tmp = x(r)
    x(r) = x(l)
    x(l) = tmp
  }

  def quickSort(x: List[Int], pivotChoice: ArrayBuffer[Int] => Int): List[Int] = {
    def go(x: ArrayBuffer[Int], l: Int, r: Int): Unit = {
      if (Math.abs(l-r) > 1) {
        // Calculate the pivot index using the pivotChoice function
        val subArray = x.drop(l).take(r)
        val pivotIndex = pivotChoice(subArray) + l

        // Ensure that the pivot is moved to the first position
        // at position x(l)
        if (pivotIndex != l) swapIndices(x, pivotIndex, l)

        var i = l+1
        var j = l+1

        // Iterate over the unpartitioned array
        for (j <- i to r) {
          // If the current value is less than the pivot
          if (x(j) < x(l)) {
            // Swap it with i, which points to the right-most number less than the pivot
            swapIndices(x, j, i)
            // Increment the i to count the <p as getting one larger
            i+=1
          }
        }
        // Move the pivot into the correct position
        swapIndices(x, l, (i-1))

        go(x, l, (i-1))
        go(x, i, r)
      }
      // Hit base case so stop recursion
    }

    val xarr = x.to(ArrayBuffer)
    go(xarr, 0, (xarr.size-1))
    xarr.toList
  }

  def main(args: Array[String]): Unit = {
    println("Hello World")
    var arr = input.to(ArrayBuffer)
    println(arr.min)

    println(arr.take(5))
    swapIndices(arr, 1, 3)
    println(arr.take(5))

    println(quickSort(input.toList, takeFirst).take(25))
  }

}
