import _root_.Nat.{Succ, Nat}

object HList {

  trait Fold[-Elem, Value] {
    type Apply[N <: Elem, -Acc <: Value] <: Value

    def apply[N <: Elem, Acc <: Value](n: N, acc: Acc): Apply[N, Acc]
  }

  sealed trait HList {
    def ::[E](e: E): HList

    def ++[L <: HList](l: L): HList

    type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value

    def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I]
  }

    sealed class HNil extends HList {
      def ::[T](v: T) = HCons(v, this)

      override def ++[L <: HList](l: L): HList = l

      override def toString = "HNil"

      override type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I

      override def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] = i
    }

    sealed case class HCons[H, T <: HList](head: H, tail: T) extends HList {
      def ::[E](v: E) = HCons(v, this)

      def ++[L <: HList](l: L) = head :: tail ++ l

      override def toString = head + " :: " + tail

      override type Foldr[Value, F <: Fold[Any, Value], I <: Value] = F#Apply[Any, T#Foldr[Value, F, I]]

      override def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] =
        f[Any, Value](head, tail.foldr[Value, F, I](f, i))
    }

    case object HNil extends HNil

    def main(args: Array[String]) {
      val list1 = "foo" :: 1 :: 1.0 :: HNil
      val list2 = "bam" :: HNil
      val list3 = list1 ++ list2

      println(list3)
      println((1 :: HNil) ++ HNil)
      println(HNil ++ ("foz" :: HNil))
    }
  }
