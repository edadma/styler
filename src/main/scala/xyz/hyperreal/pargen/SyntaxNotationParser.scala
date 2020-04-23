package xyz.hyperreal.pargen

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

object SyntaxNotationParser extends RegexParsers {

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ { _.pos }

  def syntax: Parser[SyntaxAST] = rep1(production) ^^ SyntaxAST

  def production: Parser[ProductionAST] =
    pos ~ name ~ "=" ~ pattern ~ "." ^^ {
      case pos ~ n ~ _ ~ pat ~ _ => ProductionAST(pos, n, pat)
    }

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  def pattern: Parser[PatternAST] =
    rep1sep(sequence, "|") ^^ {
      case List(e) => e
      case l       => AlternatesAST(l)
    }

  def action: Parser[ActionAST] =
    pos ~ ("<" ~> name <~ ">") ^^ {
      case p ~ n =>
        NormalActionAST(p, n)
    } |
      pos ~ ("/" ~> name) ^^ {
        case p ~ n => SpecialActionAST(p, n)
      }

  def sequence: Parser[PatternAST] =
    rep1(elem) ~ opt(action) ^^ {
      case List(e) ~ None    => e
      case List(_) ~ Some(_) => sys.error("can't have an action here")
      case l ~ a             => SequenceAST(l, a)
    }

  def elem: Parser[PatternAST] =
    pos ~ name ^^ {
      case p ~ n => IdentifierAST(p, n)
    } |
      pos ~ """"[^"\n]*"""".r ^^ {
        case p ~ s => LiteralAST(p, s.substring(1, s.length - 1))
      } |
      pos ~ ("[" ~> pattern <~ "]") ^^ {
        case pos ~ pat => OptionAST(pos, pat)
      } |
      pos ~ ("{" ~> pattern <~ "}") ^^ {
        case pos ~ pat => RepeatAST(pos, pat)
      } |
      pos ~ ("-" ~> elem) ^^ {
        case pos ~ pat => QuietAST(pos, pat)
      } |
      "(" ~> pattern <~ ")"

//  def number: Parser[LiteralAST] =
//    pos ~ """\d+(\.\d*)?""".r ^^ {
//      case p ~ n => LiteralAST(p, "number", n)
//    }

  def apply(input: String): SyntaxAST =
    parseAll(syntax, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

}
