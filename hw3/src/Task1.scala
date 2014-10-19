/**
 * User: nikita_kartashov
 * Date: 19.10.2014
 * Time: 23:40
 */

import scala.util.parsing.combinator._

object Task1 extends App {
  def evaluateDraft[T](innerFunc: (T, T) => T) = (left: Option[T], right: Option[T]) => left match {
    case None => None
    case Some(l) => right match {
      case None => None
      case Some(r) => Some(innerFunc(l, r))
    }
  }

  sealed trait Expression {
    def evaluate(): Option[Double]
  }

  class ReversedPolishParser extends JavaTokenParsers {
    abstract case class BinaryOperationExpression(left: Expression,
                                                  right: Expression,
                                                  operation: (Double, Double) => Double) extends Expression {
      override def evaluate(): Option[Double] = evaluateDraft(operation)(left.evaluate(), right.evaluate())
    }

    class Add(left: Expression, right: Expression) extends BinaryOperationExpression(left, right, _ + _)
    class Sub(left: Expression, right: Expression) extends BinaryOperationExpression(left, right, _ - _)
    class Mul(left: Expression, right: Expression) extends BinaryOperationExpression(left, right, _ * _)
    class Div(left: Expression, right: Expression) extends BinaryOperationExpression(left, right, _ / _)

    case class DoubleConstant(value: Double) extends Expression {
      override def evaluate(): Option[Double] = Some(value)
    }

    def expr: Parser[Expression] = rep(term ~ operator) ^^ {
      case terms =>
        var stack  = List.empty[Expression]
        terms.foreach {
          case nums ~ op => stack = stack ++ nums match {
            case left :: right :: rem => op(right, left) :: rem
          }
        }
        stack.head
    }
    def term: Parser[List[Expression]] = rep(num)
    def num: Parser[Expression] = floatingPointNumber ^^ (x => DoubleConstant(x.toDouble))
    def operator: Parser[(Expression, Expression) => Expression] = ("*" | "/" | "+" | "-") ^^ {
      case "+" => (x, y) => new Add(x, y)
      case "-" => (x, y) => new Sub(x, y)
      case "*" => (x, y) => new Mul(x, y)
      case "/" => (x, y) => new Div(x, y)
    }

    def performParsing(input: String) = parseAll(expr, input)
  }

  val parser = new ReversedPolishParser
  val input = "5 6 7 * - 2 +"
  val expression = parser.performParsing(input).get
  println(expression.evaluate().get)
}
