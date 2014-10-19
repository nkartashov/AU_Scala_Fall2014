/*
* Version 1.0.0
*
* Created on 23 / 09 / 2014
*
* The following text is protected by GPLv2 licence
* (http://www.gnu.org/licenses/gpl-2.0.html)
*/

object Task4 extends App {
  val operations: List[(Int, Int) => Int] = List(_+_, _*_, _-_)
  def helper(operation: (Int, Int) => Int) (left: Set[Int]) (right: Set[Int]): Set[Int] = {
    for (x <- left; y <- right) yield operation(x, y)
  }
  val setOperations = operations.map(helper)

  def applySetOperations(left: Set[Int], right: Set[Int]): Set[Int] = {
    setOperations.map(op => op(left)(right)).reduceRight(_++_)
  }

  class Interval(val left: Int, val right: Int) {
    require(left < right)
    require(left > 0)
    require(right < 12)

    override def toString: String = "(" ++ left.toString ++ ", " ++ right.toString ++ ")"

    def +(that: Interval): Interval = {
      require(this.right == that.left)
      new Interval(this.left, that.right)
    }

    def isTrivial: Boolean = (right - left) == 1

    def trivialUpdate(available: Map[Interval, Set[Int]]): Map[Interval, Set[Int]] = available + (this -> Set(left))

    def splits(): List[(Interval, Interval)] = (left + 1 until right).
      map((m: Int) => (new Interval(left, m), new Interval(m, right))).toList

    override def equals(o: Any): Boolean = o match {
      case that: Interval => this.left == that.left && this.right == that.right
      case _ => false
    }

    override def hashCode(): Int = {
      val hash = left
      hash * 31 + right
    }
  }

  def allPossibleNumbers (interval: Interval)
                         (available: Map[Interval, Set[Int]] = Map()): Map[Interval, Set[Int]] = {
    if (available contains interval) {
      available
    } else {
      if (interval.isTrivial) {
        interval.trivialUpdate(available)
      } else {
        def setMerger(left: Interval)
                     (right: Interval)
                     (known: Map[Interval, Set[Int]]): Map[Interval, Set[Int]] = {
          val newInterval = left + right
          val leftSet = known.getOrElse(left, Set())
          val rightSet = known.getOrElse(right, Set())
          assert(leftSet.nonEmpty && rightSet.nonEmpty)
          val previouslyKnown = known.getOrElse(newInterval, Set())
          val newNumbers = applySetOperations(leftSet, rightSet)
          known + (newInterval -> (newNumbers ++ previouslyKnown))
        }

        def helper(left: Interval)
                  (right: Interval)
                  (known: Map[Interval, Set[Int]]): Map[Interval, Set[Int]] = {
          val updated = allPossibleNumbers(right)(allPossibleNumbers(left)(known))
          setMerger(left)(right)(updated)
        }
        assert(!interval.isTrivial)
        interval.splits().foldRight(available)((i: (Interval, Interval), acc: Map[Interval, Set[Int]]) =>
          helper(i._1)(i._2)(acc))
      }
    }
  }

  def trial(n: Int): Boolean = {
    val fullInterval = new Interval(1, 11)
    val possible = allPossibleNumbers(fullInterval)()
    val numbers = possible.getOrElse(fullInterval, Set())
    numbers contains n
  }

  println(trial(-406))
}
