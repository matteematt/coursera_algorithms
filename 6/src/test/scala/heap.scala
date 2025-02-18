import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ArrayBuffer
import coursera.Heap

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

    "filled with values" should {

      "return None if trying to delete an arbitrary value that doesn't exist" in {
        val heap = new Heap[Int]()
        val testVals = List(20,50,60,15)
        for (testVal <- testVals) {
          heap.add(testVal)
        }
        val returned = heap.deleteArbitrary(_ == 55)
        assert(returned == None)
      }

      "return the delete value and remove it from the heap when deleting an arbitrary value" in {
        val heap = new Heap[Int]()
        val testVals = List(20,50,60,15)
        for (testVal <- testVals) {
          heap.add(testVal)
        }
        val returned = heap.deleteArbitrary(_ == 50)
        assert(returned == Some(50))
        val afterDeletion = heap.deleteArbitrary(_ == 50)
        assert(afterDeletion == None)
      }

      "overwrite a value that matches the predicate when using addUpdateArbitrary" in {
        val heap = new Heap[Int]()
        val testVals = List(20,50,60,15)
        for (testVal <- testVals) {
          heap.add(testVal)
        }
        heap.addUpdateArbitrary(_ == 50, 70)
        val returned = for (_ <- testVals) yield (heap.popMin().get)
        assert(returned == List(15,20,60,70))
      }

      "add a value that doesn't match a value with the predicate when using addUpdateArbitrary" in {
        val heap = new Heap[Int]()
        val testVals = List(20,50,60,15)
        for (testVal <- testVals) {
          heap.add(testVal)
        }
        heap.addUpdateArbitrary(_ == 55, 70)
        val returned = for (_ <- 0 to testVals.length) yield (heap.popMin().get)
        assert(returned == List(15,20,50,60,70))
      }

      "remove any values that match a predicate when using cullBy" in {
        val heap = new Heap[Int]()
        val testVals = List(20,50,60,15)
        for (testVal <- testVals) {
          heap.add(testVal)
        }
        heap.cullBy(_ < 25)
        val returned = for (_ <- 0 to 1) yield (heap.popMin().get)
        assert(returned == List(50,60))
      }
    }
  }
}
