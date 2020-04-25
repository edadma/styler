package xyz.hyperreal.styler

import scala.util.parsing.input.Position

abstract class SAST { val pos: Position }
case class SyntaxSAST(productions: Seq[ProductionSAST])                      extends SAST { val pos: Position = productions.head.pos }
case class ProductionSAST(pos: Position, name: String, pattern: PatternSAST) extends SAST

abstract class PatternSAST                        extends SAST
case class AlternatesSAST(alts: Seq[PatternSAST]) extends PatternSAST { val pos: Position = null }
case class SequenceSAST(seq: Seq[PatternSAST], action: Option[ActionSAST]) extends PatternSAST {
  val pos: Position = null
}
case class LiteralSAST(pos: Position, s: String)           extends PatternSAST
case class IdentifierSAST(pos: Position, s: String)        extends PatternSAST
case class OptionSAST(pos: Position, pattern: PatternSAST) extends PatternSAST
case class RepeatSAST(pos: Position, pattern: PatternSAST) extends PatternSAST
case class LiftSAST(pos: Position, elem: PatternSAST)      extends PatternSAST
case class AddSAST(pos: Position, elem: PatternSAST)       extends PatternSAST

abstract class ActionSAST                                 extends SAST
case class NormalActionSAST(pos: Position, name: String)  extends ActionSAST
case class SpecialActionSAST(pos: Position, name: String) extends ActionSAST
