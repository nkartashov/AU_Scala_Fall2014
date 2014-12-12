import Bool.{False, True, Bool}

/**
 * User: nikita_kartashov
 * Date: 12.12.2014
 * Time: 18:22
 */
object Nat {

  sealed trait Nat {
    type IsZero <: Bool
  }

  sealed trait Zero extends Nat {
    override type IsZero = True
  }

  sealed case class Succ[N <: Nat](n: N) extends Nat {
    override type IsZero = False
  }

}
