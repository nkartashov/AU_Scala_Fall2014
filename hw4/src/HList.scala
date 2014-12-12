

object HList {

  sealed trait HList {
    def ++[L <: HList](l: L): HList = l
  }

  sealed case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    def ::[E](v: E) = HCons(v, this)

    override def ++[L <: HList](l: L) = HCons(head, tail ++ l)

    override def toString = head + " :: " + tail
  }

  sealed class HNil extends HList {
    def ::[T](v: T) = HCons(v, this)

    override def toString = "Nil"
  }

  object HNil extends HNil

  object HList {
    type ::[H, T <: HList] = HCons[H, T]
    type ++[L1 <: HList, L2 <: HList] = HList
    val :: = HCons

    def indexAt2ofT[A, B, T <: HList](x: (A :: B :: T)) = x match {
      case a :: b :: _ => b
    }
  }

  def main(args: Array[String]) {
    import HList._


    val list1 = "foo" :: 1 :: 1.0 :: HNil
    val list2 = "bam" :: HNil
    val list3 = list1 ++ list2

    println(indexAt2ofT(list1))
    println(list3)
    println((1 :: HNil) ++ HNil)
    println(HNil ++ ("foz" :: HNil))
  }
}
