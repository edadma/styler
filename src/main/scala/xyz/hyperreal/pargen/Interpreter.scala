package xyz.hyperreal.pargen

import scala.collection.mutable

object Interpreter {

  def apply(syntax: SyntaxAST, r: Input): Node = {
    val prods = new mutable.HashMap[String, ProductionAST]

    for (p <- syntax.productions)
      prods get p.name match {
        case None    => prods(p.name) = p
        case Some(_) => problem(p.pos, s"production name has already been used: ${p.name}")
      }

    parse(syntax.productions.head.pattern, r)._1
  }

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

  private def parse(e: ElemAST, r: Input) =
    e match {
      case LiteralAST(pos, s) =>
        matches(r, s) match {
          case None       => problem(pos, "literal mismatch")
          case Some(rest) => (LiteralNode(s), rest)
        }
      case IdentifierAST(pos, s) =>
    }

}
