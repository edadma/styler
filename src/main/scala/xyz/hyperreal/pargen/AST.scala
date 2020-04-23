package xyz.hyperreal.pargen

import scala.util.parsing.input.Position

abstract class AST { val pos: Position }
case class SyntaxAST(productions: Seq[ProductionAST])                      extends AST { val pos: Position = productions.head.pos }
case class ProductionAST(pos: Position, name: String, pattern: PatternAST) extends AST

abstract class PatternAST                                               extends AST
case class AlternatesAST(alts: Seq[PatternAST])                         extends PatternAST { val pos: Position = null }
case class SequenceAST(seq: Seq[PatternAST], action: Option[ActionAST]) extends PatternAST { val pos: Position = null }
case class LiteralAST(pos: Position, s: String)                         extends PatternAST
case class IdentifierAST(pos: Position, s: String)                      extends PatternAST
case class OptionAST(pos: Position, pattern: PatternAST)                extends PatternAST
case class RepeatAST(pos: Position, pattern: PatternAST)                extends PatternAST
case class QuietAST(pos: Position, elem: PatternAST)                    extends PatternAST

abstract class ActionAST                                 extends AST
case class NormalActionAST(pos: Position, name: String)  extends ActionAST
case class SpecialActionAST(pos: Position, name: String) extends ActionAST
