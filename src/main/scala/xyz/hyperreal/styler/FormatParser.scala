package xyz.hyperreal.styler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

object FormatParser extends RegexParsers {

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ { _.pos }

  def format: Parser[SyntaxSAST] = rep1(production) ^^ SyntaxSAST

  def production: Parser[ProductionSAST] =
    pos ~ name ~ "=" ~ pattern ~ "." ^^ {
      case pos ~ n ~ _ ~ pat ~ _ => ProductionSAST(pos, n, pat)
    }

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  def pattern: Parser[PatternSAST] =
    rep1sep(sequence, "|") ^^ {
      case List(e) => e
      case l       => AlternatesSAST(l)
    }

  def action: Parser[ActionSAST] =
    pos ~ ("<" ~> name <~ ">") ^^ {
      case p ~ n =>
        NormalActionSAST(p, n)
    } |
      pos ~ ("/" ~> name) ^^ {
        case p ~ n => SpecialActionSAST(p, n)
      }

  def sequence: Parser[PatternSAST] =
    rep1(elem) ~ opt(action) ^^ {
      case List(e) ~ None    => e
      case List(_) ~ Some(_) => sys.error("can't have an action here")
      case l ~ a             => SequenceSAST(l, a)
    }

  def elem: Parser[PatternSAST] =
    pos ~ name ^^ {
      case p ~ n => IdentifierSAST(p, n)
    } |
      pos ~ """"[^"\n]*"|'[^'\n]'""".r ^^ {
        case p ~ s => LiteralSAST(p, s.substring(1, s.length - 1))
      } |
      pos ~ """`[^`\n]*`""".r ^^ {
        case p ~ s => AddSAST(p, LiteralSAST(p, s.substring(1, s.length - 1)))
      } |
      pos ~ ("[" ~> pattern <~ "]") ^^ {
        case pos ~ pat => OptionSAST(pos, pat)
      } |
      pos ~ ("{" ~> pattern <~ "}") ^^ {
        case pos ~ pat => RepeatSAST(pos, pat)
      } |
      pos ~ ("^" ~> elem) ^^ {
        case pos ~ pat => LiftSAST(pos, pat)
      } |
      pos ~ ("+" ~> elem) ^^ {
        case pos ~ pat => AddSAST(pos, pat)
      } |
      "(" ~> pattern <~ ")"

  //  def number: Parser[LiteralAST] =
  //    pos ~ """\d+(\.\d*)?""".r ^^ {
  //      case p ~ n => LiteralAST(p, "number", n)
  //    }

  def apply(input: String): SyntaxSAST =
    parseAll(syntax, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

}
