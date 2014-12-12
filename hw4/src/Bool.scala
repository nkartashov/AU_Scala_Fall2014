/**
 * User: nikita_kartashov
 * Date: 12.12.2014
 * Time: 18:23
 */
object Bool {
  sealed trait Bool {
    type If[T <: Up, F <: Up, Up] <: Up
  }

  class True extends Bool {
    type If[T <: Up, F <: Up, Up] = T
  }

  class False extends Bool {
    type If[T <: Up, F <: Up, Up] = F
  }
}
