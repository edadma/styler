package xyz.hyperreal.pargen

import scala.util.parsing.combinator.RegexParsers

object SyntaxNotationParser extends RegexParsers {

  def syntax: Parser[SyntaxAST] = rep1(production) ^^ SyntaxAST

  def production: Parser[ProductionAST] =
    name ~ "=" ~ pattern ~ "." ^^ {
      case n ~ _ ~ p ~ _ => ProductionAST(n, p)
    }

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  def identifier: Parser[IdentifierAST] = name ^^ IdentifierAST

  def pattern: Parser[ElemAST] =
    rep1sep(sequence, "|") ^^ {
      case List(e) => e
      case l       => AlternatesAST(l)
    }

  def sequence: Parser[ElemAST] =
    rep1(elem) ^^ {
      case List(e) => e
      case l       => SequenceAST(l)
    }

  def elem: Parser[ElemAST] =
    identifier |
      string |
      number |
      "[" ~> pattern <~ "]" ^^ OptionAST |
      "{" ~> pattern <~ "}" ^^ RepeatAST |
      "(" ~> pattern <~ ")"

  def number: Parser[LiteralAST] = """\d+(\.\d*)?""".r ^^ { n =>
    LiteralAST("number", n)
  }

  def string: Parser[LiteralAST] = """"[^"\n]*"""".r ^^ { s =>
    LiteralAST("string", s)
  }

  def apply(input: String): SyntaxAST = parseAll(syntax, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}
