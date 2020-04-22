package xyz.hyperreal.pargen

import scala.util.parsing.input.Position

abstract class AST { val pos: Position }
case class SyntaxAST(productions: Seq[ProductionAST])            extends AST { val pos: Position = productions.head.pos }
case class ProductionAST(pos: Position, name: String, expr: AST) extends AST

abstract class ElemAST                                             extends AST
case class AlternatesAST(terms: Seq[ElemAST])                      extends ElemAST { val pos: Position = null }
case class SequenceAST(s: Seq[ElemAST], action: Option[ActionAST]) extends ElemAST { val pos: Position = null }
case class LiteralAST(pos: Position, typ: String, s: String)       extends ElemAST
case class IdentifierAST(pos: Position, s: String)                 extends ElemAST
case class OptionAST(pos: Position, expr: ElemAST)                 extends ElemAST
case class RepeatAST(pos: Position, expr: ElemAST)                 extends ElemAST

abstract class ActionAST                                 extends AST
case class NormalActionAST(pos: Position, name: String)  extends ActionAST
case class SpecialActionAST(pos: Position, name: String) extends ActionAST
