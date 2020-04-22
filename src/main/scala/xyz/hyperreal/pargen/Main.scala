package xyz.hyperreal.pargen

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input =
    """
      |expression = term { ("+" | "-") term } /lassoc.
      |term = number | ident.
      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  Interpreter(ast, new CharSequenceReader(input))

}
