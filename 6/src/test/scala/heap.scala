import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ArrayBuffer
import dijkstra.Heap

class FirstSpec extends AnyWordSpec  {

  "A Heap" when {

    "initialised" should {

      "be empty" in {
        val heap = new Heap[Int]()
        assert(heap.isEmpty())
      }

      "return Nothing when peaking the min value" in {
        val heap = new Heap[Int]()
        assert(heap.peekMin() == None)
      }

      "return Nothing when popping the min value" in {
        val heap = new Heap[Int]()
        assert(heap.popMin() == None)
      }

    }

    "with a value" should {

      "be non-empty" in {
        val heap = new Heap[Int]()
        heap.add(5)
        assert(heap.isEmpty() == false)
      }

      "show the minimum value when using peekMin" in {
        val heap = new Heap[Int]()
        heap.add(5)
        assert(heap.peekMin() == Some(5))
        assert(heap.isEmpty() == false)
      }

      "return the minimum value when using popMin, decreasing heap size" in {
        val heap = new Heap[Int]()
        heap.add(5)
        assert(heap.popMin() == Some(5))
        assert(heap.isEmpty() == true)
      }

      "return the minimum value when using popMin, after adding a new smaller value" in {
        val heap = new Heap[Int]()
        heap.add(5)
        heap.add(3)
        assert(heap.popMin() == Some(3))
        assert(heap.isEmpty() == false)
      }
    }

    "filled with random numbers" should {
      "return the numbers in increasing order" in {
        val heap = new Heap[Int]()

        val testSetLen = 40

        val r = scala.util.Random
        for (i <- 0 to testSetLen) {
          heap.add(r.nextInt(200))
        }

        val outBuff = ArrayBuffer[Option[Int]]()
        for (i <- 0 to testSetLen) {
          outBuff += heap.popMin()
        }
        println(outBuff)

        var wrongOrdering = false
        for (i <- 0 to (testSetLen-1)) {
          wrongOrdering = wrongOrdering || ((outBuff(i),outBuff(i+1)) match {
            case (Some(x),Some(y)) if x <= y => false
            case _ => true
          })
        }

        assert(wrongOrdering == false)
      }

    }
  }
}
