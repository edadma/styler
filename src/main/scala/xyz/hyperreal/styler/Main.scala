package xyz.hyperreal.styler

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val input =
//    """
//      |a = -"[" b -"]" <array> | -"[" -"]" <array>.
//      |b = c { -"," c } /flatten.
//      |c = number | a.
//      |""".stripMargin

//    """
//      |expression = term { (`+` | `-`) term } /infixl.
//      |
//      |term = factor { (`*` | `/`) factor } /infixl.
//      |
//      |factor = unary "^" exp /infix
//      |       | unary
//      |
//      |unary = `-` primary
//      |      | primary
//      |
//      |primary = number
//      |        | ident
//      |        | "(" expression ")" <group>.
//      |""".stripMargin
    """
      |value = number | string | object | array | "true" | "false" | "null".
      |
      |object = "{" ^members "}" <object> | "{" "}" <object>.
      |
      |members = member { "," member } /flatten.
      |
      |member = string ":" value <member>.
      |
      |array = "[" ^elements "]" <array> | "[" "]" <array>.
      |
      |elements = value { "," value } /flatten.
      |""".stripMargin
  val ast = FormatParser(input)

  println(ast)
  println(Interpreter(ast, new CharSequenceReader(""" {"a": [1], "b": []} """))) // (3 + 4) * 5

}
