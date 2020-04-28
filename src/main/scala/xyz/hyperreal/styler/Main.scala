package xyz.hyperreal.styler

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

//  val jsonSyntax =
//    """
//      |value = number | string | object | array | `true` | `false` | `null`.
//      |
//      |object = "{" members "}" <object> | "{" "}" <object>.
//      |
//      |members = member { "," member } /flatten.
//      |
//      |member = string ":" value <member>.
//      |
//      |array = "[" elements "]" <array> | "[" "]" <array>.
//      |
//      |elements = value { "," value } /flatten.
//      |""".stripMargin
//  val input = """ {"things":[[123],{"things":[[],{"stuff":{"poop":"not nice","pizza":"nice"}},{}]},"asdf"]} """
//
//  val jsonFormat =
//    """
//      |printElem: {
//      |  <'number', n> -> print n;
//      |  <'string', s> -> {
//      |    print '"';
//      |    print s;
//      |    print '"';
//      |    }
//      |  ['array'] -> print '[]';
//      |  ['array', elements] -> {
//      |    printIndent '[';
//      |    printSeq elements, ',\n';
//      |    printDedent ']';
//      |    }
//      |  ['object'] -> print '{}';
//      |  ['object', members] -> {
//      |    printIndent '{';
//      |    printSeq members, ',\n';
//      |    printDedent '}';
//      |    }
//      |  ['member', key, value] -> {
//      |    printElem key;
//      |    print ': ';
//      |    printElem value;
//      |    }
//      |  <const@('true'|'false'|'null')> -> print const;
//      |}
//      |""".stripMargin
//  val sast = SyntaxParser(jsonSyntax)
//
//  val ast = StylerParser(sast, new CharSequenceReader(input)) getOrElse { println("didn't parse"); sys.exit }
//
//  val fast = FormatParser(jsonFormat)
//
//  Interpreter(fast, ast)

}
