import Bool.{False, True, Bool}

/**
 * User: nikita_kartashov
 * Date: 12.12.2014
 * Time: 18:22
 */
object Nat {

  sealed trait Nat {
    type IsZero <: Bool
    def intValue: Int

    override def toString: String = intValue.toString
  }

  case object Zero extends Nat {
    override type IsZero = True

    override def intValue: Int = 0
  }

  sealed case class Succ[N <: Nat](n: N) extends Nat {
    override type IsZero = False

    override def intValue: Int = n.intValue + 1
  }

}
