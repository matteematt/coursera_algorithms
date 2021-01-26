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

      "be non-empty after inserting a value" in {
        // val heap = new Heap()
        // heap.add(5)
        // assert(heap.isEmpty() == false)
      }
    }
  }
}
