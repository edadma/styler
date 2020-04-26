package xyz.hyperreal.styler

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

//  val input =
//    """
//      |a = -"[" b -"]" <array> | -"[" -"]" <array>.
//      |b = c { -"," c } /flatten.
//      |c = number | a.
//      |""".stripMargin

//    """
//      |value = number | string | object | array | "true" | "false" | "null".
//      |
//      |object = "{" ^members "}" <object> | "{" "}" <object>.
//      |
//      |members = member { "," member } /flatten.
//      |
//      |member = string ":" value <member>.
//      |
//      |array = "[" ^elements "]" <array> | "[" "]" <array>.
//      |
//      |elements = value { "," value } /flatten.
//      |""".stripMargin
//  val ast = SyntaxParser(input)
//
//  println(ast)
//  println(Interpreter(ast, new CharSequenceReader(""" {"a": [1], "b": []} """))) // (3 + 4) * 5

//  val input = "123"
//
//  val syntax =
//    """
//      |input = number.
//      |""".stripMargin
//  val sast = SyntaxParser(syntax)
//
////  println(sast)
//
//  val ast = StylerParser(sast, new CharSequenceReader(input)) getOrElse (sys.error("didn't parse"))
//
////  println(ast)
//
//  val format =
//    """
//      |printElem: {
//      |  ['number' n] -> print(n);
//      |}
//      |""".stripMargin
  val input = "a+b"

  val syntax =
    """
      |expression = term { (`+` | `-`) term } /infixl.
      |
      |term = factor { (`*` | `/`) factor } /infixl.
      |
      |factor = unary "^" exp /infix
      |       | unary.
      |
      |unary = `-` primary
      |      | primary.
      |
      |primary = number
      |        | ident
      |        | "(" expression ")" <group>.
      |""".stripMargin
  val sast = SyntaxParser(syntax)

  //  println(sast)

  val ast = StylerParser(sast, new CharSequenceReader(input)) getOrElse sys.error("didn't parse")

  println(ast)

  val format =
    """
      |printElem: {
      |  <'number', n> -> print(n);
      |  <'ident', v> -> print(v);
      |  ['+', left, right] -> {
      |    printElem(left);
      |    print(' plus ');
      |    printElem(right);
      |  }
      |}
      |""".stripMargin

  val fast = FormatParser(format)

//  println(fast)
  Interpreter(fast, ast)

}
