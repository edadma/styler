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

  def identifier: Parser[IdentifierAST] =
    pos ~ name ^^ {
      case p ~ n => IdentifierAST(p, n)
    }

  def pattern: Parser[ElemAST] =
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

  def sequence: Parser[ElemAST] =
    rep1(elem) ~ opt(action) ^^ {
      case List(e) ~ None    => e
      case List(_) ~ Some(_) => sys.error("can't have an action here")
      case l ~ a             => SequenceAST(l, a)
    }

  def elem: Parser[ElemAST] =
    identifier |
      literal |
      pos ~ ("[" ~> pattern <~ "]") ^^ {
        case pos ~ pat => OptionAST(pos, pat)
      } |
      pos ~ ("{" ~> pattern <~ "}") ^^ {
        case pos ~ pat => RepeatAST(pos, pat)
      } |
      "(" ~> pattern <~ ")"

//  def number: Parser[LiteralAST] =
//    pos ~ """\d+(\.\d*)?""".r ^^ {
//      case p ~ n => LiteralAST(p, "number", n)
//    }

  def literal: Parser[LiteralAST] =
    pos ~ """"[^"\n]*"""".r ^^ {
      case p ~ s => LiteralAST(p, s)
    }

  def apply(input: String): SyntaxAST =
    parseAll(syntax, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

}
