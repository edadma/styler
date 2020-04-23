package xyz.hyperreal.pargen

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input =
    """
      |expression = term { ("+" | "-") term } /infixl.
      |
      |term = factor { ("*" | "/") factor } /infixl.
      |
      |factor = number
      |       | ident
      |       | -"(" expression -")".
      |""".stripMargin

  //    """
  //      |value = number | string | object | array | "true" | "false" | "null".
  //      |
  //      |object = -"{" members -"}" <object>.
  //      |
  //      |members = member { -"," member } /flatten.
  //      |
  //      |member = string -":" value.
  //      |
  //      |array = -"[" elements -"]" <array>.
  //      |
  //      |elements = value { -"," value } /flatten.
  //      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader(""" a + b + c """)))

}
