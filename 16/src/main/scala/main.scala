package coursera

import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import scala.annotation.tailrec

case class Clause(negX: Boolean, x: Int, negY: Boolean, y: Int) {
  override def toString() = s"(${if (negX) "-" else ""}${x},${if (negY) "-" else ""}${y})"
}

case class Constraint(negX: Boolean, x: Int) {
  override def toString() = s"(${if (negX) "-" else ""}${x})"
}


object Input {
  def inputFile(x: String) = s"/home/matt/documents/coursera_algorithms/16/src/main/resources/${x}.txt"

  def parseInput(fileName: String): List[Clause] = {
    Source.fromFile(inputFile(fileName)).getLines.toList.tail
      .map { line =>
        line.split("\\s+").map(_.toInt) match {
          case Array(l,r) => Clause(l < 0, Math.abs(l) - 1, r < 0, Math.abs(r) - 1)
        }
      }
  }
}

object Reducer {

  // Also need to return the set values from eliminating the clause
  def reduce(setLen: Int, allClauses: List[Clause]): Tuple2[List[Clause],List[Constraint]] = {
    @tailrec
    def go(clauses: List[Clause], constraints: List[Constraint]):
      Tuple2[List[Clause],List[Constraint]] = {
      val setX = ArrayBuffer.fill(setLen)(false)
      val negX = ArrayBuffer.fill(setLen)(false)
      clauses.foreach { case Clause(nx, x, ny, y) =>
        if (nx) negX(x) = true else setX(x) = true
        if (ny) negX(y) = true else setX(y) = true
      }
      val fixed = (for (i <- 0 to (setLen - 1))
                    yield (i, setX(i), setX(i) ^ negX(i))
                    ).filter{ case (_,_,s) => s }.toList

      if (fixed.size == 0) (clauses,constraints)
      else {
        val newConstraints = fixed.map { case (x,setX,_) => Constraint(!setX,x) } ++ constraints
        val filterVals = fixed.map { case (x,_,_) => x }

        val filterMap = filterVals.zip(Stream.continually(List(true))).toMap
        val filteredClauses = clauses.filter { case Clause(_,x,_,y) =>
          !(filterMap.isDefinedAt(x) || filterMap.isDefinedAt(y))
        }
        go(filteredClauses,newConstraints)
      }
    }
    go(allClauses, List.empty)
  }
}

object Run {

  private val r = new Random()

  /* Imperitive way
  private def getInitial(len: Int, allConstraints: List[Constraint]): ArrayBuffer[Boolean] = {
    var constraints = allConstraints.sortBy(_.x)
    val buff = ArrayBuffer.fill(len)(false)
    var i = 0
    while (i < len) {
      if (constraints.nonEmpty && constraints.head.x == i) {
        buff(i) = !constraints.head.negX
        constraints = constraints.tail
      } else {
        buff(i) = math.random < 0.5
      }
      i += 1
    }
    buff
  }
  */

  private def getInitial(len: Int, allConstraints: List[Constraint]): ArrayBuffer[Boolean] = {
    var constraintMap = allConstraints.map{ case Constraint(negX,x) => (x,negX)}.toMap
    val buff = ArrayBuffer.fill(len)(false)
    for (i <- 0 to (len - 1)) {
      buff(i) = !constraintMap.get(i).getOrElse(math.random < 0.5)
    }
    buff
  }

  def go(len: Int, clauses: List[Clause], constraints: List[Constraint]): Option[List[Boolean]] = {
    var i = 0
    val logb2n = math.ceil(math.log10(clauses.length) / math.log10(2.0)).toInt
    val twoNSquared = math.ceil(2 * math.pow(clauses.length, 2)).toInt
    var ans: Option[List[Boolean]] = None

    while (i < logb2n) {
      // Create a random initial state, but that is valid for all reduced constraints
      val buff = getInitial(len, constraints)
      var j = 0
      while (j < twoNSquared) {
        // Split out passing and failing clauses
        val (pass,fail) = clauses.partition { case Clause(negX,x,negY,y) =>
          buff(x) != negX || buff(y) != negY
        }
        if (fail.length == 0) {
          // We have found a valid solution
          ans = Some(buff.toList)
          j += twoNSquared
          i += logb2n
        } else {
          // Choose a random failing clause
          val clause = r.shuffle(fail).head
          // An improvement of the lecture pseudocode is to flip the failing bit
          // in the clause rather than a random one
          if (buff(clause.x) == clause.negX) buff(clause.x) = !buff(clause.x)
          else buff(clause.y) = !buff(clause.y)
        }
        j += 1
      }
      i += 1
      println(s"Completed ${i} / $logb2n => ${((((i) / logb2n)) * 100).toInt}%")
    }
    ans
  }

  def run(filename: String): Option[List[Boolean]] = {
    val input = Input.parseInput(filename)
    // Get the number of variables, +1 because I reduce all numbers by 1
    // to 0 index the variables
    val setLen = input.flatMap{ case Clause(_,x,_,y) => List(x,y)}.max + 1
    val (clauses, constraints) = Reducer.reduce(setLen, input)
    Run.go(setLen, clauses, constraints)
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("\n\nPapadimitriou's Algorithmn\n")
    val files = if (args.length == 0) List("tc1","tc2") else args.toList
    val ans = files.map(x => Run.run(x))
    println(ans)
  }
}
