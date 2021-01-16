package quicksort

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {

  val filePath = "/home/matt/documents/coursera_algorithms/3/src/main/resources/input.txt"
  val input: List[Int] = Source.fromFile(filePath).getLines.toList.map(_.toInt)

  def takeFirst(x: ArrayBuffer[Int]): Int = 0

  def takeLast(x: ArrayBuffer[Int]): Int = x.length - 1

  def medianOfThree(x: ArrayBuffer[Int]): Int = {
    var middleIndex = if (x.length % 2 == 0) ((x.length/2)-1) else (x.length/2)
    var lastIndex = x.length - 1
    var choicePairs = Array((0, x(0)), (middleIndex, x(middleIndex)), (lastIndex, x(lastIndex)))
    val median = choicePairs.map(_._2).sortWith(_ < _).drop(choicePairs.length/2).head
    choicePairs.filter({ case (index,value) => value == median}).head._1
  }

  def swapIndices(x: ArrayBuffer[Int], l: Int, r: Int): Unit = {
    val tmp = x(r)
    x(r) = x(l)
    x(l) = tmp
  }

  def quickSort(x: List[Int], pivotChoice: ArrayBuffer[Int] => Int): (Int,List[Int]) = {
    var comparisons = 0

    def go(x: ArrayBuffer[Int], l: Int, r: Int): Unit = {
      if (Math.abs(l-r) > 1) {
        // Increment the number of comparisons made
        comparisons += Math.abs(l-r) - 1

        // Calculate the pivot index using the pivotChoice function
        val subArray = x.drop(l).take(r-l)
        val pivotIndex = pivotChoice(subArray) + l

        // Ensure that the pivot is moved to the first position
        // at position x(l)
        if (pivotIndex != l) swapIndices(x, pivotIndex, l)

        var i = l+1
        var j = l+1

        // Iterate over the unpartitioned array
        for (j <- i to (r-1)) {
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
    go(xarr, 0, (xarr.size))
    (comparisons, xarr.toList)
  }

  def main(args: Array[String]): Unit = {
    var (firstCount, firstSorted) = quickSort(input.toList, takeFirst)
    println("=====Take First=====")
    println(firstSorted.take(5))
    println(firstCount)

    var (lastCount, lastSorted) = quickSort(input.toList, takeLast)
    println("=====Take last=====")
    println(lastSorted.take(5))
    println(lastCount)

    var (motCount, motSorted) = quickSort(input.toList, medianOfThree)
    println("=====Take mot=====")
    println(motSorted.take(5))
    println(motCount)

  }

}
