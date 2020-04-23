package xyz.hyperreal.pargen

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input = """expression = "+"."""
//    """
//      |expression = term { ("+" | "-") term } /lassoc.
//      |term = number | ident.
//      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader("+")))

}
