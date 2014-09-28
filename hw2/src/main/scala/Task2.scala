package main.scala

/**
 * Task2
 *
 * Version 1.0.0
 *
 * Created on 23/09/2014
 *
 * The following text is protected by GPLv2 licence
 * (http://www.gnu.org/licenses/gpl-2.0.html)
 **/

object Task2 extends App {
  private def slider(l: List[Int]): List[List[Int]] = {
    if (l.length < 2) {
      List()
    } else {
      l.sliding(2).toList
    }
  }

  private def mapper(l: List[Int]): List[Int] = 1 :: slider(l).map((ll: List[Int]) => ll.sum) ++ List(1)
  val result:Stream[List[Int]] = List(1) #:: result.map(mapper)
  println((result take 5).toList)
}
