import org.scalatest.wordspec.AnyWordSpec

import dijkstra.Heap

class FirstSpec extends AnyWordSpec  {

  "A Heap" when {

    "initialised" should {

      "be isEmpty" in {
        val heap = new Heap()
      }
    }
  }
}
