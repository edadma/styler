package xyz.hyperreal.styler

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

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
  val input = "  a+((( 5-c ))) * d / e   "

  val syntax =
    """
      |expression = term { (`+` | `-`) term } /infixl.
      |
      |term = factor { (`*` | `/`) factor } /infixl.
      |
      |factor = unary `^` factor /infix
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
      |  <'ident'|'number', lit> -> print(lit);
      |  [op@('+'|'-'), left, right] -> {
      |    printElem(left);
      |    print(' ');
      |    print(op);
      |    print(' ');
      |    printElem(right);
      |    }
      |  [op@('*'|'/'), left, right] -> {
      |    printElem(left);
      |    print(op);
      |    printElem(right);
      |    }
      |  ['group', inner@['group', _]] -> printElem(inner);
      |  ['group', expr] -> {
      |    print('(');
      |    printElem(expr);
      |    print(')');
      |    }
      |}
      |""".stripMargin

  val fast = FormatParser(format)

//  println(fast)
  Interpreter(fast, ast)

}
