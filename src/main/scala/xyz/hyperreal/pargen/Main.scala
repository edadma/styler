package xyz.hyperreal.pargen

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input = //"""expression = term { ("+" | "-") term } /infixl. term = number | ident."""
    """
      |expression = term { ("+" | "-") term } /infixl.
      |
      |term = factor { ("*" | "/") factor } /infixl.
      |
      |factor = number
      |       | ident
      |       | -"(" expression -")".
      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader("a + b * c")))

}
