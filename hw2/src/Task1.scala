/**
 * Task1
 *
 * Version 1.0.0
 *
 * Created on 23/09/2014
 *
 * The following text is protected by GPLv2 licence
 * (http://www.gnu.org/licenses/gpl-2.0.html)
 */

object Task1 extends App {
  private def y(p: BigInt, x: Int): BigInt = x * (20 * p + x)

  private def trial(p: BigInt, c: BigInt): Int = {
    ((0 to 9).reverse find (x => y(p, x) <= c)).get
  }

  val squareSeed = (List(1), 100: BigInt)

  private def worker(computedSoFar: List[Int])(c: BigInt): (List[Int], BigInt) = {
    val p = BigInt(computedSoFar.reverse.mkString)
    val newX = trial(p, c)
    def mapper(x: Int): (List[Int], BigInt) =
      (x :: computedSoFar, (c - y(p, x)) * 100)
    mapper(newX)
  }

  def compute(n: Int): List[Int] =
    (Stream.iterate(squareSeed)(t => worker(t._1)(t._2)) map (x => x._1) take (n + 1)).last.reverse

  print(compute(10))
}
