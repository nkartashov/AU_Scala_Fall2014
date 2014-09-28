/*
* Version 1.0.0
*
* Created on 09 / 09 / 2014
*
* The following text is protected by GPLv2 licence
* (http://www.gnu.org/licenses/gpl-2.0.html)
*/

object Task1 {
  def gcd(x: Int, y: Int): Int = {
    if (x < y) {
      gcd(y, x)
    } else {
      if (y == 0) {
        x
      } else {
        gcd(y, x % y)
      }
    }
  }

  def fibonacci(n: Int) = fibonacci_helper(0, 1, n)

  def fibonacci_helper(first: Int, second:Int, current: Int): Int = {
    if (current == 0) {
      first
    } else {
      fibonacci_helper(second, first + second, current - 1)
    }
  }

  def qsort[A <% Ordered[A]](data: List[A]): List[A] = {
    data match {
      case x :: xs =>
        val partitioned = xs.partition(xx => xx <= x)
        qsort(partitioned._1) ++ List(x) ++ qsort(partitioned._2)
      case _ => data
    }
  }

  def main(args: Array[String]) {
    println(gcd(3, 7))
    val l = List(1, 2, 3, 7, 9, 0)
    println(qsort(l))
    println((1 to 10).map(fibonacci))
  }

}
