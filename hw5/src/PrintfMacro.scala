import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * User: nikita_kartashov
 * Date: 13.12.2014
 * Time: 14:57
 */
object PrintfMacro {
  def printf(format: String, params: Any*): Unit = macro printfImpl

  def printfImpl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    val Literal(Constant(formatString: String)) = format.tree
    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }
    val paramsStack = mutable.Stack[Tree](params map (_.tree): _*)
    val refs = formatString.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop(), typeOf[Int])
      case "%s" => precompute(paramsStack.pop(), typeOf[String])
      case "%c" => precompute(paramsStack.pop(), typeOf[Char])
      case "%f" => precompute(paramsStack.pop(), typeOf[Float])
      case part => Literal(Constant(part))
    }
    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }

}
