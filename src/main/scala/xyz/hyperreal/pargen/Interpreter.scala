package xyz.hyperreal.pargen

import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Interpreter {

  private val builtin =
    Map[String, Input => Option[(String, Input)]](
      "number" -> numberMatcher,
      "ident"  -> identMatcher,
      "string" -> stringMatcher
    )

  def apply(syntax: SyntaxAST, r: Input): Option[Node] = {
    val rules = new mutable.HashMap[String, PatternAST]

    def parse(e: PatternAST, r: Input): Option[(Node, Input)] =
      e match {
        case RepeatAST(pos, pattern) =>
          val buf = new ListBuffer[Node]

          @scala.annotation.tailrec
          def repeat(r: Input): (BranchNode, Input) =
            parse(pattern, r) match {
              case None => (BranchNode("rep", buf.toList), r)
              case Some((n, r)) =>
                buf += n
                repeat(r)
            }

          Some(repeat(r))
        case AlternatesAST(alts) =>
          @scala.annotation.tailrec
          def alternative(alts: Seq[PatternAST]): Option[(Node, Input)] =
            alts match {
              case Nil => None
              case h :: t =>
                parse(h, r) match {
                  case None => alternative(t)
                  case res  => res
                }
            }

          alternative(alts)
        case SequenceAST(seq, action) =>
          val buf = new ListBuffer[Node]

          @scala.annotation.tailrec
          def sequence(s: Seq[PatternAST], r: Input): Option[(Node, Input)] =
            s match {
              case Nil =>
                if (action isDefined) {
                  action.get match {
                    case NormalActionAST(pos, name) => Some((BranchNode(name, buf.toList), r))
                    case SpecialActionAST(pos, "infixl") =>
                      val tree =
                        buf(1).asInstanceOf[BranchNode].nodes.foldLeft(buf.head) {
                          case (a, BranchNode("seq", Seq(LiteralNode(op), b))) =>
                            BranchNode(op, Seq(a, b))
                          case _ => problem(pos, "invalid pattern for 'infixl' special action")
                        }

                      Some((tree, r))
                  }
                } else if (buf.length == 1)
                  Some((buf.head, r))
                else
                  Some(BranchNode("seq", buf.toList), r)
              case h :: t =>
                parse(h, r) match {
                  case None => None
                  case Some((n, r)) =>
                    if (!h.isInstanceOf[QuietAST])
                      buf += n

                    sequence(t, r)
                }
            }

          sequence(seq, r)
        case QuietAST(pos, e) => parse(e, r)
        case LiteralAST(pos, s) =>
          matches(r, s) map (rest => (LiteralNode(s), skipSpace(rest))) // todo: problem(pos, "literal mismatch")
        case IdentifierAST(pos, s) =>
          rules get s match {
            case None =>
              builtin get s match {
                case None => problem(pos, s"unknown rule: $s")
                case Some(matcher) =>
                  matcher(r) map { case (m, rest) => (LeafNode(s, m), skipSpace(rest)) } // todo: problem(pos, s"failed to match a '$s'")
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

  private val numberCharSet = Set('.', 'e', 'E', '-', '+') ++ ('0' to '9')
  private val numberRegex   = """(?:\d+\.\d+|\.\d+|\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r.pattern

  private def numberMatcher(r: Input) = regexMatcher(r, numberCharSet, numberCharSet, numberRegex)

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
