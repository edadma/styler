package xyz.hyperreal.pargen

import scala.collection.mutable
import scala.util.parsing.input.{CharSequenceReader, Reader}

object Interpreter {

  def apply(syntax: SyntaxAST, r: Reader[Char]) = {
    val prods = new mutable.HashMap[String, ProductionAST]

    for (p <- syntax.productions)
      prods get p.name match {
        case None    => prods(p.name) = p
        case Some(_) => problem(p.pos, s"production name has already been used: ${p.name}")
      }

    elem(syntax.productions.head.pattern, r)
  }

  def elem(e: ElemAST, r: Reader[Char]): Node = {}

}
