package xyz.hyperreal.styler

import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StylerParser {

  private val builtin =
    Map[String, Input => Option[(String, Input)]](
      "number" -> numberMatcher,
      "ident"  -> identMatcher,
      "string" -> stringMatcher
    )

  def apply(syntax: SyntaxSAST, r: Input): Option[Elem] = {
    val rules = new mutable.HashMap[String, PatternSAST]

    def parse(e: PatternSAST, r: Input): Option[(Elem, Input)] =
      e match {
        case RepeatSAST(pos, pattern) =>
          val buf = new ListBuffer[Elem]

          @scala.annotation.tailrec
          def repeat(r: Input): (BranchElem, Input) =
            parse(pattern, r) match {
              case None => (BranchElem("rep", List(BranchElem("items", buf.toList))), r)
              case Some((n, r)) =>
                buf += n
                repeat(r)
            }

          Some(repeat(r))
        case OptionSAST(pos, pattern) =>
          parse(pattern, r) match {
            case None => Some((BranchElem("opt", Nil), r))
            case s    => s
          }
        case AlternatesSAST(alts) =>
          @scala.annotation.tailrec
          def alternative(alts: Seq[PatternSAST]): Option[(Elem, Input)] =
            alts match {
              case Nil => None
              case h :: t =>
                parse(h, r) match {
                  case None => alternative(t)
                  case res  => res
                }
            }

          alternative(alts)
        case SequenceSAST(seq, action) =>
          val buf = new ListBuffer[Elem]

          def lift =
            buf.toList flatMap {
              case LiftElem(BranchElem(_, seq)) => seq
              case _: LiftElem                  => problem(null, "can only lift a sequence")
              case e                            => List(e)
            }

          @scala.annotation.tailrec
          def sequence(s: Seq[PatternSAST], r: Input): Option[(Elem, Input)] =
            s match {
              case Nil =>
                if (action isDefined) {
                  action.get match {
                    case NormalActionSAST(pos, name) => Some((BranchElem(name, lift), r))
                    case SpecialActionSAST(pos, "infixl") =>
                      val tree =
                        buf(1)
                          .asInstanceOf[BranchElem]
                          .branches
                          .head
                          .asInstanceOf[BranchElem]
                          .branches
                          .foldLeft(buf.head) {
                            case (a, BranchElem("seq", Seq(StringElem(op), b))) =>
                              BranchElem(op, Seq(a, b))
                            case _ => problem(pos, "invalid pattern for 'infixl' special action")
                          }

                      Some((tree, r))
                    case SpecialActionSAST(pos, "flatten") =>
                      def flatten(l: List[Elem]): List[Elem] =
                        l flatMap {
                          case BranchElem("rep", List(BranchElem(_, nodes))) => nodes
                          case n                                             => List(n)
                        }

                      Some((BranchElem("seq", flatten(buf.toList)), r))
                  }
                } else if (buf.length == 1)
                  Some((buf.head, r))
                else
                  Some(BranchElem("seq", lift), r)
              case h :: t =>
                parse(h, r) match {
                  case None => None
                  case Some((n, r)) =>
                    if (!h.isInstanceOf[LiteralSAST])
                      buf += n

                    sequence(t, r)
                }
            }

          sequence(seq, r)
        case AddSAST(pos, e)  => parse(e, r)
        case LiftSAST(pos, e) => nodewrap(parse(e, r), LiftElem)
        case LiteralSAST(pos, s) =>
          matches(r, s) map (rest => (StringElem(s), skipSpace(rest))) // todo: problem(pos, "literal mismatch")
        case IdentifierSAST(pos, s) =>
          rules get s match {
            case None =>
              builtin get s match {
                case None => problem(pos, s"unknown rule: $s")
                case Some(matcher) =>
                  matcher(r) map { case (m, rest) => (LeafElem(s, m), skipSpace(rest)) } // todo: problem(pos, s"failed to match a '$s'")
              }
            case Some(rule) => parse(rule, r)
          }
      }

    for (p <- syntax.productions)
      rules get p.name match {
        case None    => rules(p.name) = p.pattern
        case Some(_) => problem(p.pos, s"production name has already been used: ${p.name}")
      }

    parse(syntax.productions.head.pattern, skipSpace(r)) match {
      case None => None
      case Some((n, r)) =>
        if (r.atEnd)
          Some(n)
        else
          None
    }
  }

  private def nodewrap(res: Option[(Elem, Input)], wrapper: Elem => Elem) =
    res map {
      case (n, i) => (wrapper(n), i)
    }

  @scala.annotation.tailrec
  private def skipSpace(r: Input): Input =
    if (!r.atEnd && r.first.isWhitespace)
      skipSpace(r.rest)
    else
      r

  private def stringMatcher(r: Input) = {
    csMatcher(r, _ == '"', _ != '"') match {
      case None => None
      case Some((s, r)) =>
        if (r.atEnd)
          None
        else
          Some((s substring 1, r.rest))
    }
  }

  private def csMatcher(r: Input, init: Char => Boolean, rest: Char => Boolean) = {
    val buf = new StringBuilder

    if (r.atEnd || !init(r.first))
      None
    else {
      @scala.annotation.tailrec
      def csMatcher(r: Input): Option[(String, Input)] =
        if (r.atEnd || !rest(r.first))
          Some((buf.toString, r))
        else {
          buf += r.first
          csMatcher(r.rest)
        }

      buf += r.first
      csMatcher(r.rest)
    }
  }

  private def identMatcher(r: Input) =
    csMatcher(r, c => c.isLetter || c == '_', c => c.isLetterOrDigit || c == '_')

  private def regexMatcher(r: Input, init: Char => Boolean, rest: Char => Boolean, p: Pattern) =
    csMatcher(r, init, rest) match {
      case None => None
      case res @ Some((m, r1)) =>
        if (p.matcher(m).matches)
          res
        else
          None
    }

  //private val numberRegex   = """(?:\d+\.\d+|\.\d+|\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r.pattern

  val digit  = SetLexer(_.isDigit)
  val digits = Rep1Lexer(digit)
  val dot    = CharLexer('.')

  abstract class Lexer
  case class CharLexer(c: Char)                  extends Lexer
  case class SetLexer(set: Char => Boolean)      extends Lexer
  case class RepLexer(lex: Lexer)                extends Lexer
  case class Rep1Lexer(lex: Lexer)               extends Lexer
  case class SeqLexer(left: Lexer, right: Lexer) extends Lexer
  case class AltLexer(left: Lexer, right: Lexer) extends Lexer
  case class OptLexer(lex: Lexer)                extends Lexer

  def matches(r: Input, lex: Lexer) = {
    @scala.annotation.tailrec
    def rep(r: Input, lex: Lexer, buf: Vector[Char]): Option[(Input, Vector[Char])] =
      lexer(r, lex, buf) match {
        case None           => Some((r, buf))
        case Some((r, buf)) => rep(r, lex, buf)
      }

    def lexer(r: Input, lex: Lexer, buf: Vector[Char]): Option[(Input, Vector[Char])] =
      lex match {
        case CharLexer(c) =>
          if (!r.atEnd && c == r.first)
            Some((r.rest, buf :+ r.first))
          else
            None
        case SetLexer(set) =>
          if (!r.atEnd && set(r.first))
            Some((r.rest, buf :+ r.first))
          else
            None
        case OptLexer(lex) =>
          lexer(r, lex, buf) match {
            case None => Some((r, buf))
            case s    => s
          }
        case RepLexer(lex) => rep(r, lex, buf)
        case Rep1Lexer(lex) =>
          lexer(r, lex, buf) match {
            case None           => None
            case Some((r, buf)) => rep(r, lex, buf)
          }
        case SeqLexer(left, right) =>
          lexer(r, left, buf) match {
            case None           => None
            case Some((r, buf)) => lexer(r, right, buf)
          }
        case AltLexer(left, right) =>
          lexer(r, left, buf) match {
            case None => lexer(r, right, buf)
            case s    => s
          }
      }

    lexer(r, lex, Vector.empty) map { case (r, v) => (v mkString, r) }
  }

  private def numberMatcher(r: Input) =
    matches(
      r,
      SeqLexer(
        AltLexer(SeqLexer(digits, OptLexer(SeqLexer(dot, digits))), SeqLexer(dot, digits)),
        OptLexer(SeqLexer(SetLexer(Set('e', 'E')), SeqLexer(OptLexer(SetLexer(Set('+', '-'))), digits)))
      )
    )

  private def matches(r: Input, s: String) = {
    @scala.annotation.tailrec
    def matches(r: Input, idx: Int): Option[Input] =
      if (idx == s.length)
        Some(r)
      else if (r.atEnd || r.first != s(idx))
        None
      else
        matches(r.rest, idx + 1)

    matches(r, 0)
  }

}
