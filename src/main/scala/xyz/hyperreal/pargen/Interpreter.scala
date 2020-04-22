package xyz.hyperreal.pargen

import scala.collection.mutable

object Interpreter {

  def apply(syntax: SyntaxAST) = {
    val start = syntax.productions.head
    val prods = new mutable.HashMap[String, ProductionAST]

    for (p <- syntax.productions)
      prods get p.name match {
        case None    => prods(p.name) = p
        case Some(_) => problem(p.pos, s"production name has already been used: ${p.name}")
      }
  }

}
