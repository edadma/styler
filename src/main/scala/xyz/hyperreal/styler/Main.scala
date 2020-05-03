package xyz.hyperreal.styler

import java.io.PrintStream

import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  case class Options(
      source: String = null,
      syntax: String = null,
      out: String = null
  )

  private val optionsParser = new scopt.OptionParser[Options]("styler") {
    head("Styler", "v0.1.0")
    arg[String]("<source file>")
      .action((x, c) => c.copy(source = x))
      .text("source file to format (- refers to standard input)")
    arg[String]("<syntax/format file>")
      .action((x, c) => c.copy(syntax = x))
    help("help").text("print this usage text").abbr("h")
    opt[String]('o', "out")
      .optional()
      .valueName("<output file>")
      .action((x, c) => c.copy(out = x))
      .text("optional output file")
    version("version")
      .text("print the version")
      .abbr("v")
  }

//  optionsParser.parse(args, Options()) match {
//    case Some(options) =>
//      val s =
//        if (options.source.toString == "-")
//          io.Source.stdin
//        else
//          io.Source.fromFile(options.source)
//
//      val input = readSource(s)
//
//      s.close
//
//      val out =
//        if (options.out ne null)
//          new PrintStream(options.out)
//        else
//          Console.out
//      val syn = readFile(options.syntax ++ ".syn")
//      val fmt = readFile(options.syntax ++ ".fmt")
//
//      val sast = SyntaxParser(syn)
//      val ast  = StylerParser(sast, new CharSequenceReader(input)) getOrElse { println("didn't parse"); sys.exit(1) }
//      val fast = FormatParser(fmt)
//
//      Interpreter(fast, ast, out)
//    case None => sys.exit(1)
//  }

  def readFile(f: String) = readSource(io.Source.fromFile(f))

  def readSource(s: io.Source) = {
    val res = s.mkString

    s.close
    res
  }

  val syn =
    """
      |syntax = rule* :rules.
      |
      |rule = ident "=" pattern "." :rule.
      |
      |pattern = rep1sep(seq, "|").
      |
      |seq = quant* action? :seq.
      |
      |quant = primary "*" :star | primary "+" :plus | primary "?" :quest | primary.
      |
      |primary = ("rep1sep"|"repsep") "(" pattern "," pattern ")" | ident | string | "(" pattern ")" | "{" pattern "}" :rep |"[" pattern "]" :opt.
      |
      |action = ":" ident :name | "/" ident :special
      |       | "->" element :element.
      |
      |element = "[" repsep(element, ",") "]" :element | ident | string | int | "..." int :spread.
      |""".stripMargin

  val fmt =
    """
      |eq = 0;
      |
      |printElem: {
      |  ['rules', rules] -> printSeq rules, '\n';
      |  ['rule', ['ident', name], patterns] -> {
      |    print name;
      |    print ' ';
      |    eq = col;
      |    print '= ';
      |    printSeq patterns, {
      |      print '\n';
      |      printSpace eq;
      |      print '| ';
      |      };
      |    print '.\n';
      |    }
      |  ['seq', items, action] -> {
      |    printSeq items, ' ';
      |    printAction action;
      |    }
      |  ['ident', name] -> print name;
      |  ['string', string] -> {
      |    print '"';
      |    print string;
      |    print '"';
      |    }
      |  ['star', pat] -> {
      |    printElem pat;
      |    print '*';
      |    }
      |  ['plus', pat] -> {
      |    printElem pat;
      |    print '+';
      |    }
      |  ['ques', pat] -> {
      |    printElem pat;
      |    print '?';
      |    }
      |}
      |
      |printAction: {
      |  [] -> {}
      |  ['name', ['ident', name]] -> {
      |    print ' :';
      |    print name;
      |    }
      |  ['special', ['ident', name]] -> {
      |    print ' /';
      |    print name;
      |    }
      |  ['element', element] -> {
      |    printElem element;
      |    }
      |}
      |""".stripMargin

  val input =
//    """
//      |syntax = rule* :rules.
//      |
//      |rule = ident "=" pattern "." :rule.
//      |
//      |pattern = rep1sep(seq, "|").
//      |
//      |seq = item* action? :seq.
//      |
//      |item = ident | string.
//      |
//      |action = ":" ident :name | "/" ident :special
//      |       | "->" element :element.
//      |
//      |element = "[" repsep(element, ",") "]" :element | ident | string | int | "..." int :spread.
//      |""".stripMargin
    """
      |syntax = rule* :rules.
      |
      |rule = ident "=" pattern "." :rule.
      |
      |pattern = rep1sep(seq, "|").
      |""".stripMargin
  val sast = SyntaxParser(syn)
  val ast  = StylerParser(sast, new CharSequenceReader(input)) getOrElse { println("didn't parse"); sys.exit(1) }

  println(ast)
  val fast = FormatParser(fmt)

  Interpreter(fast, ast, Console.out)

}
