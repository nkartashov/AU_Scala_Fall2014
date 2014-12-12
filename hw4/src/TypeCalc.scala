object TypeCalc {

  sealed trait TypeBool {
    type If[TrueType <: Up, FalseType <: Up, Up] <: Up
  }

  class TypeTrue extends TypeBool {
    type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
  }

  class TypeFalse extends TypeBool {
    type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
  }

  type X[T <: TypeBool] = T#If[String, Int, Any]

  val x : X[TypeTrue] = "5"
  val y : X[TypeFalse] = 5
}
