package main.scala

/**
 * Task3
 *
 * Version 1.0.0
 *
 * Created on 23/09/2014
 *
 * The following text is protected by GPLv2 licence
 * (http://www.gnu.org/licenses/gpl-2.0.html)
 */

object Task3 extends App {
  abstract class Logical {
    def reduce(): Boolean
    def ==(that: Logical): Boolean = this.reduce() == that.reduce()
  }
  case class True() extends Logical {
    override def reduce(): Boolean = true
  }
  case class False() extends Logical {
    override def reduce(): Boolean = false
  }
  case class X(x: Boolean) extends Logical {
    override def reduce(): Boolean = x
  }
  case class Not(x: Logical) extends Logical {
    override def reduce(): Boolean = !x.reduce()
  }
  case class And(left: Logical, right: Logical) extends Logical {
    override def reduce(): Boolean = left.reduce() && right.reduce()
  }
  case class Or(left: Logical, right: Logical) extends Logical {
    override def reduce(): Boolean = left.reduce() || right.reduce()
  }

  val x = X(x = true)
  val y = X(x = false)
  val xory = Or(x, y)
  val z:Logical = False()
  val notzandxory = And(Not(z), xory)
  println((True():Logical) == notzandxory)
}
