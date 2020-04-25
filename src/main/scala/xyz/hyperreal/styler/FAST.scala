package xyz.hyperreal.styler

import scala.util.parsing.input.Position

abstract class FAST
case class FormatFAST(decls: DeclarationFAST) extends FAST

abstract class DeclarationFAST                                                extends FAST { val name: String; val pos: Position }
case class VariableDeclaration(pos: Position, name: String, value: ValueFAST) extends DeclarationFAST
case class FunctionDeclaration(pos: Position, name: String, body: CasesFAST)  extends DeclarationFAST

case class CasesFAST(cases: Seq[(PatternFAST, StatementFAST)]) extends FAST
case class ValueFAST(v: Any)                                   extends FAST

abstract class PatternFAST                                               extends FAST
trait SimplePattern                                                      extends PatternFAST
case class VariablePattern(name: String)                                 extends PatternFAST with SimplePattern
case class StringPattern(s: String)                                      extends PatternFAST with SimplePattern
case class LeafPattern(typ: SimplePattern, value: SimplePattern)         extends PatternFAST
case class BranchPattern(typ: SimplePattern, branches: Seq[PatternFAST]) extends PatternFAST

abstract class StatementFAST                                  extends FAST
case class BlockStatement(stmts: Seq[StatementFAST])          extends StatementFAST
case class ApplyStatement(func: String, args: Seq[ValueFAST]) extends StatementFAST
