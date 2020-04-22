package xyz.hyperreal.pargen

abstract class AST
case class SyntaxAST(productions: Seq[ProductionAST]) extends AST
case class ProductionAST(name: String, expr: AST)     extends AST

abstract class ElemAST                                             extends AST
case class AlternatesAST(terms: Seq[ElemAST])                      extends ElemAST
case class SequenceAST(s: Seq[ElemAST], action: Option[ActionAST]) extends ElemAST
case class LiteralAST(typ: String, s: String)                      extends ElemAST
case class IdentifierAST(s: String)                                extends ElemAST
case class OptionAST(expr: ElemAST)                                extends ElemAST
case class RepeatAST(expr: ElemAST)                                extends ElemAST

abstract class ActionAST                  extends AST
case class NormalActionAST(name: String)  extends ActionAST
case class SpecialActionAST(name: String) extends ActionAST
