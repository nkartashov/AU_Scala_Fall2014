import scala.language.experimental.macros

/**
 * User: nikita_kartashov
 * Date: 13.12.2014
 * Time: 14:28
 */

object Macro extends App {
  import PrintfMacro._

  printf("My name is %s %s!\n", "Nikita", "Kartashov")
  printf("I'm %d years old\n", 22)
  printf("My height is %f cm\n", 177.2f)
  printf("Printf works: %c\n", 'T')
}
