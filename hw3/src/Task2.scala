import scala.util.parsing.combinator.JavaTokenParsers

/**
 * User: nikita_kartashov
 * Date: 20.10.2014
 * Time: 01:09
 */
object Task2 extends App {

  sealed trait JasonObject {
    override def toString = prettyJason
    def prettyJason: String
  }

  class JasonParser extends JavaTokenParsers {
    case class JasonStruct(struct: List[(JasonString, JasonObject)]) extends JasonObject {
      override def prettyJason: String =
        "{" ++ struct.map(x => x._1.toString ++ ": " ++ x._2.toString).mkString(", \n") ++ "}"
    }
    case class JasonArray(array: List[JasonObject]) extends JasonObject {
      override def prettyJason: String = "[" ++ array.mkString(", ") ++ "]"
    }
    case class JasonString(string: String) extends JasonObject {
      override def prettyJason: String = string
    }
    case class JasonDouble(double: Double) extends JasonObject {
      override def prettyJason: String = double.toString
    }
    case class JasonNull() extends JasonObject {
      override def prettyJason: String = "null"
    }
    case class JasonBool(boolean: Boolean) extends JasonObject {
      override def prettyJason: String = boolean.toString
    }

    def jobject:Parser[JasonObject] = jstruct | jarray | jstring | jdouble | jbool | jnull
    def jstruct:Parser[JasonStruct] = "{" ~> repsep(jproperty, ",") <~ "}" ^^ JasonStruct
    def jproperty:Parser[(JasonString, JasonObject)] = jstring ~ ":" ~ jobject ^^ {
      case key ~ ":" ~ value => (key, value)
    }
    def jarray:Parser[JasonArray] = "[" ~> repsep(jobject, ",") <~ "]" ^^ JasonArray
    def jstring:Parser[JasonString] = stringLiteral ^^ JasonString
    def jdouble:Parser[JasonDouble] = floatingPointNumber ^^ (x => JasonDouble(x.toDouble))
    def jbool:Parser[JasonBool] = ("true" | "false") ^^ {
      case "true" => JasonBool(boolean = true)
      case _ => JasonBool(boolean = false)
    }
    def jnull:Parser[JasonNull] = "null" ^^ (_ => JasonNull.apply())

    def readJason(input: String) = parseAll(jstruct, input)
  }

  val parser = new JasonParser
  val jason = """{
    "firstName": "Иван",
    "lastName": "Иванов",
    "address": {
    "streetAddress": "Московское ш., 101, кв.101",
    "city": "Ленинград",
    "postalCode": 101101
    },
    "phoneNumbers": [
    "812 123-1234",
    "916 123-4567"
    ]
    }"""
  println(parser.readJason(jason))
}
