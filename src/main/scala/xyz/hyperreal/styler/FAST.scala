package xyz.hyperreal.styler

import scala.util.parsing.input.Position

abstract class FAST
case class FormatFAST(decls: List[DeclarationFAST]) extends FAST

abstract class DeclarationFAST extends FAST { val name: String; val pos: Position }
case class VariableDeclaration(pos: Position, name: String, init: ExpressionFAST, var value: Any = null)
    extends DeclarationFAST
case class FunctionDeclaration(pos: Position, name: String, body: CasesFAST) extends DeclarationFAST

case class CasesFAST(cases: Seq[(PatternFAST, StatementFAST)]) extends FAST

abstract class ExpressionFAST                              extends FAST
case class LiteralExpression(literal: Any)                 extends ExpressionFAST
case class VariableExpression(pos: Position, name: String) extends ExpressionFAST

abstract class PatternFAST                                                              extends FAST
trait SimplePattern                                                                     extends PatternFAST
case class VariablePattern(pos: Position, name: String)                                 extends PatternFAST with SimplePattern
case class StringPattern(pos: Position, s: String)                                      extends PatternFAST with SimplePattern
case class LeafPattern(pos: Position, typ: SimplePattern, value: SimplePattern)         extends PatternFAST
case class BranchPattern(pos: Position, typ: SimplePattern, branches: Seq[PatternFAST]) extends PatternFAST

abstract class StatementFAST                                                      extends FAST
case class BlockStatement(stmts: Seq[StatementFAST])                              extends StatementFAST
case class ApplyStatement(pos: Position, func: String, args: Seq[ExpressionFAST]) extends StatementFAST
