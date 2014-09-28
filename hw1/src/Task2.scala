/*
* Version 1.0.0
*
* Created on 09 / 09 / 2014
*
* The following text is protected by GPLv2 licence
* (http://www.gnu.org/licenses/gpl-2.0.html)
*/

object Task2 {
  abstract class Scheme
  case class Resistance(resistance: Int) extends Scheme
  case class Sequence(before: Scheme, after: Scheme) extends Scheme
  case class Parallel(left: Scheme, right: Scheme) extends Scheme

  def padTo(input: String, desiredWidth: Int): String = input + " " * (desiredWidth - input.length)

  def padList(input: List[String]) = input.map(padTo(_: String, maxWidth(input)))

  def maxWidth(input: List[String]): Int = input.map(s => s.length).max

  def horizontalLine(desiredWidth: Int): String = "-" * desiredWidth

  def verticalLine(desiredHeight: Int): List[String] = (0 to desiredHeight).map(_ => "|").toList

  def wrapWithVerticalLines(input: List[String]) = "|" :: input ++ List("|")

  def wrapWithHorizontalLines(input: List[String]): List[String] = {
    val resultWidth = maxWidth(input)
    List(horizontalLine(resultWidth)) ++ input ++ List(horizontalLine(resultWidth))
  }
  
  def elongateShorter(left: List[String], right: List[String]): (List[String], List[String]) = {
    val difference = left.length - right.length
    if (difference > 0) {
      (left, right ++ verticalLine(difference))
    } else if (difference < 0) {
      (left ++ verticalLine(-difference), right)
    } else {
      (left, right)
    }
  }

  def show(scheme: Scheme) = {
    val lines = showHelper(scheme)
    for (line <- lines) {
      println(line)
    }
  }

  def showHelper(scheme: Scheme): List[String] = scheme match {
    case Resistance(resistance) =>
      wrapWithVerticalLines(List(resistance.toString))
    case Sequence(before, after) =>
      val stringsBefore = showHelper(before)
      val stringsAfter = showHelper(after)
      val resultingWidth = Math.max(maxWidth(stringsBefore), maxWidth(stringsAfter))
      (stringsBefore ++ stringsAfter).map(padTo(_: String, resultingWidth))
    case Parallel(left, right) =>
      val stringsLeft = showHelper(left)
      val stringsRight = showHelper(right)
      val elongatedPair = elongateShorter(stringsLeft, stringsRight)
      def crazyMap[A, B](inputTuple: (A, A), mapper: A => B): (B, B) = (mapper(inputTuple._1), mapper(inputTuple._2))
      def crazierMapper(t: (String, String)): String = t._1 + "     " + t._2
      def totlaMad[A, B](inputTuple: (List[A], List[B])): List[(A, B)] = inputTuple._1.zip(inputTuple._2)
      val preResult = totlaMad(crazyMap(elongatedPair, padList)).map(crazierMapper)
      padList(wrapWithVerticalLines(wrapWithHorizontalLines(preResult)))
  }

  def main(args: Array[String]) {
    val r1 = Resistance(10)
    val r2 = Resistance(150)
    val r3 = Resistance(1000)
    val r31 = Resistance(200)
    val r32 = Resistance(1)
    val r4 = Resistance(3000)
    val seq1 = Sequence(r1, r2)
    val seq2 = Sequence(seq1, r31)
    val seq3 = Sequence(r3, r32)
    val par1 = Parallel(seq2, seq3)
    val par2 = Parallel(r4, par1)
    show(par2)
  }

}
