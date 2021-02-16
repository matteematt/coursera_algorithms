package coursera

import org.scalatest.wordspec.AnyWordSpec

class UnionFindSpec extends AnyWordSpec {

  "A UnionFind" when {

    "initialised" should {

      "have 0 sets" in {
        val uf = new UnionFind[Int]()
        assert(uf.setCount == 0)
      }

      "return None when trying to find a value" in {
        val uf = new UnionFind[Int]()
        assert(uf.find(23) == None)
      }

      "do nothing when trying to merge nonexistent sets" in {
        val uf = new UnionFind[Int]()
        uf.merge(12,24)
        assert(true)
      }

      "be able to retrieve the leader and have 1 set after adding a value" in {
        val uf = new UnionFind[Int]()
        uf.makeSet(12)
        assert(uf.setCount == 1)
        assert(uf.find(12) == Some(12))
      }
    }

    "filled with two singletons" should {

      "have 2 sets" in {
        val uf = new UnionFind[Int]()
        uf.makeSet(12)
        uf.makeSet(13)
        assert(uf.setCount == 2)
      }

      "have 1 set after merging the two values" in {
        val uf = new UnionFind[Int]()
        uf.makeSet(12)
        uf.makeSet(13)
        uf.merge(12,13)
        assert(uf.setCount == 1)
      }

      "have 1 set after trying to merge the values again" in {
        val uf = new UnionFind[Int]()
        uf.makeSet(12)
        uf.makeSet(13)
        uf.merge(12,13)
        uf.merge(12,13)
        assert(uf.setCount == 1)
      }
    }

    "filled with two merged values" should {

      "have the same parent when finding" in {
        val uf = new UnionFind[Int]()
        uf.makeSet(12)
        uf.makeSet(13)
        uf.merge(12,13)
        assert(uf.find(12) == uf.find(13))
      }
    }

    "filled with multiple singletons" should {

      "decrease the sets after every new merge, and then the values should have the same leader" in {
        val uf = new UnionFind[Int]()
        0 to 6 foreach (x => uf.makeSet(x))

        var setCount = 7
        assert(uf.setCount == setCount)

        1 to 6 foreach { x =>
          assert(uf.find(0) != uf.find(x))
          uf.merge(0,x)
          assert(uf.find(0) == uf.find(x))
          assert(uf.setCount == (setCount - 1))
          setCount -= 1
        }
      }
    }
  }

}
