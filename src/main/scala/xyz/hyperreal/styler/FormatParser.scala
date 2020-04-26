package xyz.hyperreal.styler

import java.io

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

object FormatParser extends RegexParsers {

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ { _.pos }

  def format: Parser[FormatFAST] = rep1(declaration) ^^ FormatFAST

  def declaration: Parser[DeclarationFAST] =
    variable | function

  def variable: Parser[VariableDeclaration] =
    pos ~ name ~ "=" ~ expression ^^ {
      case p ~ n ~ _ ~ v => VariableDeclaration(p, n, v)
    }

  def function: Parser[FunctionDeclaration] =
    pos ~ name ~ ":" ~ cases ^^ {
      case p ~ n ~ _ ~ a => FunctionDeclaration(p, n, cases)
    }

  def cases: Parser[CasesFAST] =
    "{" ~> rep1(arrow) <~ "}" ^^ CasesFAST

  def arrow: Parser[(PatternFAST, StatementFAST)] =
    pattern ~ "->" ~ statement ^^ {
      case p ~ _ ~ s => (p, s)
    }

  def statement: Parser[StatementFAST] =
    simpleStatement <~ ";" |
      "{" ~> rep(simpleStatement <~ ";") <~ "}" ^^ BlockStatement

  def simpleStatement: Parser[StatementFAST] =
    pos ~ name ~ "(" ~ rep(expression) ~ ")" ^^ {
      case p ~ n ~ _ ~ args ~ _ => ApplyStatement(p, n, args)
    }

  def variablePattern: Parser[VariablePattern] =
    pos ~ name ^^ {
      case p ~ n => VariablePattern(p, n)
    }

  def string: Parser[String] = """"[^"\n]*"|'[^'\n]'""".r

  def stringPattern: Parser[VariablePattern] =
    pos ~ name ^^ {
      case p ~ n => VariablePattern(p, n)
    }

  def simplePattern: Parser[SimplePattern] = variablePattern | stringPattern

  def pattern: Parser[PatternFAST] =
    variablePattern |
      pos ~ "[" ~ simplePattern ~ simplePattern ~ "]" ^^ {
        case p ~ _ ~ n ~ v ~ _ => LeafPattern(p, n, v)
      }

  def expression: Parser[ExpressionFAST] =
    pos ~ """"[^"\n]*"|'[^'\n]'""".r ^^ {
      case p ~ s => LiteralExpression(p, s.substring(1, s.length - 1))
    } |
      pos ~ """(?:\d+\.\d+|\.\d+|\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r ^^ {
        case p ~ n => LiteralExpression(p, n.toDouble)
      } |
      pos ~ name ^^ {
        case p ~ n => VariableExpression(p, n)
      }

  def name: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

//  def alternate: Parser[PatternSAST] =
//    rep1sep(sequence, "|") ^^ {
//      case List(e) => e
//      case l       => AlternatesSAST(l)
//    }
//
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
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

}
