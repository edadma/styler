package xyz.hyperreal.styler

import java.io.PrintStream

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
      val s =
        if (options.source.toString == "-")
          io.Source.stdin
        else
          io.Source.fromFile(options.source)

      val input = readSource(s)

      s.close

      val out =
        if (options.out ne null)
          new PrintStream(options.out)
        else
          Console.out
      val syn = readFile(options.syntax ++ ".syn")
      val fmt = readFile(options.syntax ++ ".fmt")

      val sast = SyntaxParser(syn)
      val ast  = StylerParser(sast, new CharSequenceReader(input)) getOrElse { println("didn't parse"); sys.exit(1) }
      val fast = FormatParser(fmt)

      Interpreter(fast, ast, out)
    case None => sys.exit(1)
  }

  def readFile(f: String) = readSource(io.Source.fromFile(f))

  def readSource(s: io.Source) = {
    val res = s.mkString

    s.close
    res
  }

}
