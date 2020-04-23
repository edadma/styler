package xyz.hyperreal.pargen

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input = """array = -"[" ident { -"," ident } -"]" /flatten."""
//    """
//      |value = number | string | object | array | "true" | "false" | "null".
//      |
//      |object = "{" members "}".
//      |
//      |members = member { -"," member } /flatten.
//      |
//      |member = string -":" value <member>.
//      |
//      |array = value { -"," value } /flatten.
//      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader(""" "asdf" """)))

}
