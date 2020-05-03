package xyz.hyperreal.styler

import scala.util.parsing.input.Position

abstract class SAST
case class SyntaxSAST(productions: Seq[ProductionSAST])                      extends SAST
case class ProductionSAST(pos: Position, name: String, pattern: PatternSAST) extends SAST

abstract class PatternSAST                                                             extends SAST
case class AlternativePatternSAST(alts: Seq[PatternSAST])                              extends PatternSAST
case class ConcatenationPatternSAST(seq: Seq[PatternSAST], action: Option[ActionSAST]) extends PatternSAST
case class LiteralPatternSAST(pos: Position, s: String)                                extends PatternSAST
case class QuietLiteralPatternSAST(pos: Position, s: String)                           extends PatternSAST
case class IdentifierPatternSAST(pos: Position, s: String)                             extends PatternSAST
case class OptionPatternSAST(pos: Position, pattern: PatternSAST)                      extends PatternSAST
case class RepeatPatternSAST(pos: Position, pattern: PatternSAST)                      extends PatternSAST
case class Repeat1PatternSAST(pos: Position, pattern: PatternSAST)                     extends PatternSAST
case class RepsepPatternSAST(pos: Position, pattern: PatternSAST, sep: PatternSAST)    extends PatternSAST
case class Rep1sepPatternSAST(pos: Position, pattern: PatternSAST, sep: PatternSAST)   extends PatternSAST

abstract class ActionSAST                                      extends SAST
case class ElementActionSAST(pos: Position, elem: ElementSAST) extends ActionSAST
case class NameActionSAST(pos: Position, name: String)         extends ActionSAST
case class SpecialActionSAST(pos: Position, action: String)    extends ActionSAST

abstract class ElementSAST                            extends SAST
case class StringElementSAST(s: String)               extends ElementSAST
case class RefElementSAST(ref: Int)                   extends ElementSAST
case class SpreadElementSAST(pos: Position, ref: Int) extends ElementSAST
case class ListElementSAST(elems: Seq[ElementSAST])   extends ElementSAST
