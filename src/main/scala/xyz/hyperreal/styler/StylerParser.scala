package xyz.hyperreal.styler

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StylerParser {

  private val builtin =
    Map[String, Input => Option[(String, Input)]](
      "number" -> numberMatcher,
      "int"    -> intMatcher,
      "ident"  -> identMatcher,
      "string" -> stringMatcher
    )

  def apply(syntax: SyntaxSAST, r: Input): Option[Elem] = {
    val rules = new mutable.HashMap[String, PatternSAST]

    def parse(e: PatternSAST, r: Input): Option[(Elem, Input)] = {
      //      println("parse", e, r)

      @scala.annotation.tailrec
      def repeat(pattern: PatternSAST, r: Input, buf: ListBuffer[Elem] = new ListBuffer): (ListElem, Input) =
        parse(pattern, r) match {
          case None => (ListElem(buf.toList), r)
          case Some((n, r)) =>
            buf += n
            repeat(pattern, r, buf)
        }

      def rep1sep(pattern: PatternSAST, sep: PatternSAST, r: Input, buf: ListBuffer[Elem] = new ListBuffer) = {
        @scala.annotation.tailrec
        def rep1sep(r: Input): Option[(ListElem, Input)] = {
          parse(sep, r) match {
            case None => Some((ListElem(buf.toList), r))
            case Some((_, r1)) =>
              parse(pattern, r1) match {
                case None => Some((ListElem(buf.toList), r))
                case Some((n, r)) =>
                  buf += n
                  rep1sep(r)
              }
          }
        }

        parse(pattern, r) match {
          case None => None
          case Some((e, r)) =>
            buf += e
            rep1sep(r)
        }
      }

      e match {
        case RepsepPatternSAST(pos, pattern, sep) =>
          rep1sep(pattern, sep, r) match {
            case None => Some(NilElem, r)
            case s    => s
          }
        case Rep1sepPatternSAST(pos, pattern, sep) => rep1sep(pattern, sep, r)
        case RepeatPatternSAST(pos, pattern)       => Some(repeat(pattern, r))
        case Repeat1PatternSAST(pos, pattern) =>
          val buf = new ListBuffer[Elem]

          parse(pattern, r) match {
            case None => None
            case Some((e, r)) =>
              buf += e
              Some(repeat(pattern, r, buf))
          }
        case OptionPatternSAST(pos, pattern) =>
          parse(pattern, r) match {
            case None => Some((NilElem, r))
            case s    => s
          }
        case AlternativePatternSAST(alts) =>
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
        case ConcatenationPatternSAST(seq, action) =>
          val buf = new ListBuffer[Elem]

          @scala.annotation.tailrec
          def sequence(s: Seq[PatternSAST], r: Input): Option[(Elem, Input)] =
            s match {
              case Nil =>
                if (action isDefined) {
                  action.get match {
                    case NameActionSAST(pos, name) => Some((ListElem(StringElem(name) +: buf.toList), r))
                    case SpecialActionSAST(pos, "infixl") =>
                      val tree =
                        buf(1)
                          .asInstanceOf[ListElem]
                          .elems
                          .foldLeft(buf.head) {
                            case (a, ListElem(Seq(op: StringElem, b))) =>
                              ListElem(Seq(op, a, b))
                            case _ => problem(pos, "invalid pattern for 'infixl' special action")
                          }

                      Some((tree, r))
                    case ElementActionSAST(pos, elem) =>
                      def mkElem(e: ElementSAST): Elem =
                        e match {
                          case ListElementSAST(elems) =>
                            ListElem(elems flatMap {
                              case SpreadElementSAST(_, ref) => buf(ref - 1).asInstanceOf[ListElem].elems
                              case e                         => List(mkElem(e))
                            })
                          case RefElementSAST(ref) => buf(ref - 1)
                          case SpreadElementSAST(pos, ref) =>
                            problem(pos, "spread operator can only be used inside a list")
                          case StringElementSAST(s) => StringElem(s)
                        }

                      Some(mkElem(elem), r)

                    //                    case SpecialActionSAST(pos, "flatten") =>
                    //                      def flatten(l: List[Elem]): List[Elem] =
                    //                        l flatMap {
                    //                          case ListElem(nodes) => nodes
                    //                          case n               => List(n)
                    //                        }
                    //
                    //                      Some((ListElem(flatten(buf.toList)), r))
                  }
                } else if (buf.length == 1)
                  Some((buf.head, r))
                else
                  Some(ListElem(buf.toList), r)
              case h :: t =>
                parse(h, r) match {
                  case None => None
                  case Some((n, r)) =>
                    if (!(h.isInstanceOf[LiteralPatternSAST] && h.asInstanceOf[LiteralPatternSAST].quiet))
                      buf += n

                    sequence(t, r)
                }
            }

          sequence(seq, r)
        case LiteralPatternSAST(pos, s, _) => matches(r, s) map (rest => (StringElem(s), skipSpace(rest)))
        case IdentifierPatternSAST(pos, s) =>
          rules get s match {
            case None =>
              builtin get s match {
                case None => problem(pos, s"unknown rule: $s")
                case Some(matcher) =>
                  matcher(r) map { case (m, rest) => (ListElem(List(StringElem(s), StringElem(m))), skipSpace(rest)) }
              }
            case Some(rule) => parse(rule, r)
          }
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
          None // problem(r.pos, "expected end of input")
    }
  }

  //  private def nodewrap(res: Option[(Elem, Input)], wrapper: Elem => Elem) =
  //    res map {
  //      case (n, i) => (wrapper(n), i)
  //    }

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

  private def intMatcher(r: Input) =
    csMatcher(r, _.isDigit, _.isDigit)

  //private val numberRegex   = """(?:\d+\.\d+|\.\d+|\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r.pattern

  val digit: SetLexer   = SetLexer(_.isDigit)
  val digits: Rep1Lexer = Rep1Lexer(digit)
  val dot: CharLexer    = CharLexer('.')

  abstract class Lexer
  case class CharLexer(c: Char)                  extends Lexer
  case class SetLexer(set: Char => Boolean)      extends Lexer
  case class RepLexer(lex: Lexer)                extends Lexer
  case class Rep1Lexer(lex: Lexer)               extends Lexer
  case class SeqLexer(left: Lexer, right: Lexer) extends Lexer
  case class AltLexer(left: Lexer, right: Lexer) extends Lexer
  case class OptLexer(lex: Lexer)                extends Lexer

  def matches(r: Input, lex: Lexer): Option[(String, Input)] = {
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
