package xyz.hyperreal.styler

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input =
//    """
//      |a = -"[" b -"]" <array> | -"[" -"]" <array>.
//      |b = c { -"," c } /flatten.
//      |c = number | a.
//      |""".stripMargin
    """
      |expression = term { (`+` | `-`) term } /infixl.
      |
      |term = factor { (`*` | `/`) factor } /infixl.
      |
      |factor = number
      |       | ident
      |       | "(" expression ")".
      |""".stripMargin
//    """
//      |value = number | string | object | array | "true" | "false" | "null".
//      |
//      |object = "{" members "}" <object> | "{" "}" <object>.
//      |
//      |members = member { "," member } /flatten.
//      |
//      |member = string ":" value <kv>.
//      |
//      |array = "[" elements "]" <array> | "[" "]" <array>.
//      |
//      |elements = value { "," value } /flatten.
//      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader(""" (3 + 4) * 5 """))) //{"a": [1], "b": []}

}
