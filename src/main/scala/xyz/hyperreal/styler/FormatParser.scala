package xyz.hyperreal.styler

import java.io

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

object FormatParser extends RegexParsers {
  val escapes = "\\\\b|\\\\f|\\\\t|\\\\r|\\\\n|\\\\\\\\|\\\\\"" r

  def unescape(s: String) =
    escapes.replaceAllIn(s, _.matched match {
      case "\\b"  => "\b"
      case "\\f"  => "\f"
      case "\\t"  => "\t"
      case "\\r"  => "\r"
      case "\\n"  => "\n"
      case "\\\\" => "\\"
      case "\\\"" => "\""
    })

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ { _.pos }

  def format: Parser[FormatFAST] =
    rep1(declaration) ^^ FormatFAST

  def declaration: Parser[DeclarationFAST] =
    variable | function

  def variable: Parser[VariableDeclaration] =
    pos ~ name ~ "=" ~ expression ~ ";" ^^ {
      case p ~ n ~ _ ~ v ~ _ => VariableDeclaration(p, n, v)
    }

  def function: Parser[FunctionDeclaration] =
    pos ~ name ~ ":" ~ cases ^^ {
      case p ~ n ~ _ ~ cs => FunctionDeclaration(p, n, cs)
    }

  def cases: Parser[Seq[(PatternFAST, StatementFAST)]] =
    "{" ~> rep1(arrow) <~ "}"

  def arrow: Parser[(PatternFAST, StatementFAST)] =
    pattern ~ "->" ~ statement ^^ {
      case p ~ _ ~ s => (p, s)
    }

  def statement: Parser[StatementFAST] =
    simpleStatement <~ ";" |
      "{" ~> rep(simpleStatement <~ ";") <~ "}" ^^ BlockStatement

  def simpleStatement: Parser[StatementFAST] =
    pos ~ name ~ "=" ~ pos ~ expression ^^ {
      case p ~ n ~ _ ~ ep ~ e => AssignmentStatement(p, n, ep, e)
    } |
      pos ~ name ~ repsep(expression, ",") ^^ {
        case p ~ n ~ args => ApplyStatement(p, n, args)
      }

  def string: Parser[String] = """"[^"\n]*"|'[^'\n]*'""".r ^^ (s => unescape(s.substring(1, s.length - 1)))

  def pattern: Parser[PatternFAST] = alternate

  def alternate: Parser[PatternFAST] =
    rep1sep(namedPattern, "|") ^^ {
      case List(e) => e
      case l       => AlternatesPattern(l)
    }

  def namedPattern: Parser[PatternFAST] =
    pos ~ name ~ "@" ~ primaryPattern ^^ {
      case p ~ n ~ _ ~ pat => NamedPattern(p, n, pat)
    } |
      primaryPattern

  def primaryPattern: Parser[PatternFAST] =
    pos ~ name ^^ {
      case p ~ n => VariablePattern(p, n)
    } |
      pos ~ string ^^ {
        case p ~ s => StringPattern(p, s)
      } |
      pos ~ "<" ~ pattern ~ ">" ^^ {
        case p ~ _ ~ pat ~ _ => LiteralPattern(p, pat)
      } |
      pos ~ "<" ~ pattern ~ "," ~ pattern ~ ">" ^^ {
        case p ~ _ ~ n ~ _ ~ v ~ _ => LeafPattern(p, n, v)
      } |
      pos ~ "[" ~ pattern ~ opt("," ~> rep1sep(pattern, ",")) ~ "]" ^^ {
        case p ~ _ ~ n ~ None ~ _     => BranchPattern(p, n, Nil)
        case p ~ _ ~ n ~ Some(bs) ~ _ => BranchPattern(p, n, bs)
      } |
      "_" ^^^ AnyPattern |
      "(" ~> pattern <~ ")"

  def expression: Parser[ExpressionFAST] =
    pos ~ string ^^ {
      case p ~ s => LiteralExpression(p, s)
    } |
      pos ~ """(?:\d+\.\d+|\.\d+|\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r ^^ {
        case p ~ n => LiteralExpression(p, n.toDouble)
      } |
      pos ~ name ^^ {
        case p ~ n => VariableExpression(p, n)
      } |
      pos ~ ("{" ~> rep(simpleStatement <~ ";") <~ "}") ^^ {
        case p ~ s => BlockExpression(p, s)
      } |
      "(" ~> expression <~ ")"

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

//  def sequence: Parser[PatternSAST] =
//    rep1(elem) ^^ {
//      case List(e) => e
//      case l       => SequenceSAST(l)
//    }
//
//  def elem: Parser[PatternSAST] =
//    pos ~ name ^^ {
//      case p ~ n => IdentifierSAST(p, n)
//    } |
//      pos ~ """"[^"\n]*"|'[^'\n]'""".r ^^ {
//        case p ~ s => LiteralSAST(p, s.substring(1, s.length - 1))
//      } |
//      pos ~ """`[^`\n]*`""".r ^^ {
//        case p ~ s => AddSAST(p, LiteralSAST(p, s.substring(1, s.length - 1)))
//      } |
//      pos ~ ("[" ~> pattern <~ "]") ^^ {
//        case pos ~ pat => OptionSAST(pos, pat)
//      } |
//      pos ~ ("{" ~> pattern <~ "}") ^^ {
//        case pos ~ pat => RepeatSAST(pos, pat)
//      } |
//      pos ~ ("^" ~> elem) ^^ {
//        case pos ~ pat => LiftSAST(pos, pat)
//      } |
//      pos ~ ("+" ~> elem) ^^ {
//        case pos ~ pat => AddSAST(pos, pat)
//      } |
//      "(" ~> pattern <~ ")"

  def apply(input: String): FormatFAST =
    parseAll(format, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        println(failure)
        sys.exit
    }

}
