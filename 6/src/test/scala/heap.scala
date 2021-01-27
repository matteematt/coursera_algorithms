import org.scalatest.wordspec.AnyWordSpec

import dijkstra.Heap

class FirstSpec extends AnyWordSpec  {

  "A Heap" when {

    "initialised" should {

      "be empty" in {
        val heap = new Heap()
        assert(heap.isEmpty())
      }

      "return Nothing when peaking the min value" in {
        val heap = new Heap()
        assert(heap.peekMin() == None)
      }

      "return Nothing when popping the min value" in {
        val heap = new Heap()
        assert(heap.popMin() == None)
      }

    }

    "with a value" should {

      "be non-empty" in {
        val heap = new Heap()
        heap.add(5)
        assert(heap.isEmpty() == false)
      }

      "show the minimum value when using peekMin" in {
        val heap = new Heap()
        heap.add(5)
        assert(heap.peekMin() == Some(5))
        assert(heap.isEmpty() == false)
      }

      "return the minimum value when using popMin, decreasing heap size" in {
        val heap = new Heap()
        heap.add(5)
        assert(heap.popMin() == Some(5))
        assert(heap.isEmpty() == true)
      }

      "return the minimum value when using popMin, after adding a new smaller value" in {
        val heap = new Heap()
        heap.add(5)
        heap.add(3)
        assert(heap.popMin() == Some(3))
        assert(heap.isEmpty() == false)
      }
    }

    "filled with random numbers" should {
      "return the numbers in increasing order" in {
        val heap = new Heap()

        val r = scala.util.Random
        val randomVals = Iterator.unfold(0){
          case x if x < 40 => Some((r.nextInt(200), x+1))
          case _ => None
        }
        // randomVals.fold(heap){
          // (x: Heap,h: Int) => h.add(x)
        // }
        randomVals.fold(heap) {
          (h,i) => h.add(i)
        }
        println(randomVals.mkString(","))
      }
    }
  }
}
