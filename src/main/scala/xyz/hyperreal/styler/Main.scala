package xyz.hyperreal.styler

import scala.reflect.io.File
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
//      .validate(
//        x =>
//          if (!x.exists)
//            failure(s"not found: $x")
//          else if (x.isFile && x.canRead)
//            success
//          else
//            failure(s"unreadable: $x"))
      .text("source file to format (- refers to standard input)")
    arg[String]("<syntax/format file>")
      .action((x, c) => c.copy(syntax = x))
    help("help").text("print this usage text").abbr("h")
    opt[String]('o', "out")
      .optional()
      .valueName("<output file>")
      .action((x, c) => c.copy(out = x))
//      .validate(
//        x =>
//          if (!x.exists || x.canWrite)
//            success
//          else
//            failure(s"Option --out: can't write to $x"))
      .text("optional output file")
    version("version")
      .text("print the version")
      .abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
      if (options.parser) {
        val asl = parse(options.files).head

        if (options.out ne null)
          write(asl.toString, options.out)
        else
          println(asl)
      } else if (options.gen) {
        val code = generate(options.files, options.opt)

        if (options.out ne null)
          write(code, options.out)
        else
          println(code)
      } else if (options.run) {
        interp(options.files)
      } else if (options.source) {
        val code = source(options.files, options.opt)

        if (options.out ne null)
          write(code, options.out)
        else
          println(code)
      } else if (options.out ne null)
        executable(options.out, options.files, options.opt)
      else
        executable(new File("executable"), options.files, options.opt)
    case None => sys.exit(1)
  }

  def sourceForFile(f: File) =
    if (f.toString == "-")
      io.Source.stdin
    else
      io.Source.fromFile(f)

  def readFile(f: File) = {
    val s   = io.Source.fromFile(f)
    val res = s.mkString

    s.close
    res
  }

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
