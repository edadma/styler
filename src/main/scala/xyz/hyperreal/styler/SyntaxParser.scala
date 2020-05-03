package xyz.hyperreal.styler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

object SyntaxParser extends RegexParsers {

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ { _.pos }

  def syntax: Parser[SyntaxSAST] = rep1(production) ^^ SyntaxSAST

  def production: Parser[ProductionSAST] =
    pos ~ name ~ "=" ~ pattern ~ "." ^^ {
      case pos ~ n ~ _ ~ pat ~ _ => ProductionSAST(pos, n, pat)
    }

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  def pattern: Parser[PatternSAST] =
    rep1sep(sequence, "|") ^^ {
      case List(e) => e
      case l       => AlternativePatternSAST(l)
    }

  def sequence: Parser[PatternSAST] =
    pos ~ rep1(postfixPattern) ~ opt(action) ^^ {
      case _ ~ List(e) ~ None => e
      case _ ~ l ~ a          => ConcatenationPatternSAST(l, a)
    }

  def action: Parser[ActionSAST] =
    pos ~ ("/" ~> name) ^^ {
      case p ~ n => SpecialActionSAST(p, n)
    } |
      pos ~ (":" ~> name) ^^ {
        case p ~ n => NameActionSAST(p, n)
      } |
      pos ~ ("->" ~> element) ^^ {
        case p ~ e => ElementActionSAST(p, e)
      }

  def int: Parser[Int] = """\d+""".r ^^ (_.toInt)

  def element: Parser[ElementSAST] =
    "[" ~> repsep(element, ",") <~ "]" ^^ ListElementSAST |
      string ^^ StringElementSAST |
      int ^^ RefElementSAST |
      pos ~ "..." ~ int ^^ {
        case p ~ _ ~ r => SpreadElementSAST(p, r)
      }

  def string: Parser[String] = """"[^"\n]*"|'[^'\n]'""".r ^^ (s => s.substring(1, s.length - 1))

  def postfixPattern: Parser[PatternSAST] =
    pos ~ (primaryPattern <~ "?") ^^ {
      case pos ~ pat => OptionPatternSAST(pos, pat)
    } |
      pos ~ (primaryPattern <~ "*") ^^ {
        case pos ~ pat => RepeatPatternSAST(pos, pat)
      } |
      pos ~ (primaryPattern <~ "+") ^^ {
        case pos ~ pat => Repeat1PatternSAST(pos, pat)
      } |
      primaryPattern

  def primaryPattern: Parser[PatternSAST] =
    pos ~ ("rep1sep" ~> "(" ~> (pattern ~ "," ~ pattern) <~ ")") ^^ {
      case pos ~ (pat ~ _ ~ sep) => Rep1sepPatternSAST(pos, pat, sep)
    } |
      pos ~ ("repsep" ~> "(" ~> (pattern ~ "," ~ pattern) <~ ")") ^^ {
        case pos ~ (pat ~ _ ~ sep) => RepsepPatternSAST(pos, pat, sep)
      } |
      pos ~ name ^^ {
        case p ~ n => IdentifierPatternSAST(p, n)
      } |
      pos ~ string ^^ {
        case p ~ s => QuietLiteralPatternSAST(p, s)
      } |
      pos ~ """`[^`\n]*`""".r ^^ {
        case p ~ s => LiteralPatternSAST(p, s.substring(1, s.length - 1))
      } |
      pos ~ ("[" ~> pattern <~ "]") ^^ {
        case pos ~ pat => OptionPatternSAST(pos, pat)
      } |
      pos ~ ("{" ~> pattern <~ "}") ^^ {
        case pos ~ pat => RepeatPatternSAST(pos, pat)
      } |
      "(" ~> pattern <~ ")"

  def apply(input: String): SyntaxSAST =
    parseAll(syntax, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        println(failure)
        sys.exit
    }

}
