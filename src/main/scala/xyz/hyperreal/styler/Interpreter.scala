package xyz.hyperreal.styler

import scala.collection.mutable
import scala.util.parsing.input.Position

object Interpreter {

  private val builtins =
    List[(String, Seq[Any] => Unit)](
      "print" -> (args => println(args mkString ", "))
    )

  def apply(ast: FAST, elem: Elem): Unit = {
    val builtinDecls =
      builtins map {
        case (n, f) => (n, NativeDeclaration(n, f))
      }
    val declsMap = mutable.HashMap[String, DeclarationFAST](builtinDecls: _*)

    def apply(ast: FAST): Unit = {
      def eval(expr: ExpressionFAST): Any =
        expr match {
          case LiteralExpression(literal) => literal
          case VariableExpression(pos, name) =>
            declsMap get name match {
              case Some(VariableDeclaration(_, _, _, value)) => value
              case None                                      => problem(pos, "variable not found")
            }
          case _ =>
        }

      def call(pos: Position, func: String, args: Seq[Any]): Unit =
        declsMap get func match {
          case Some(FunctionDeclaration(_, _, cases)) =>
            val locals = new mutable.HashMap[String, Any]

            def execute(stmt: StatementFAST): Unit =
              stmt match {
                case ApplyStatement(pos, func, args) => call(pos, func, args map eval)
                case BlockStatement(stmts)           => stmts foreach execute
                case _                               =>
              }

            val arg = if (args.length == 1) args.head else args

            def unify( pat: PatternFAST )
            def matchCases(cases: Seq[(PatternFAST, StatementFAST)]): Unit =
              cases match {
                case Nil               => problem(pos, "none of the cases matched")
                case Some((pat, stmt)) =>
              }

            matchCases(cases)
          case _ => problem(pos, "function not declared")
        }

      ast match {
        case FormatFAST(decls) =>
          decls foreach apply
          decls foreach {
            case v @ VariableDeclaration(_, _, init, _) => v.value = eval(init)
            case _                                      =>
          }
          call(null, "printElem", Seq(elem))
        case decl @ VariableDeclaration(pos, name, value, _) =>
          declsMap get name match {
            case Some(_) => problem(pos, "name already used")
            case None    => declsMap(name) = decl
          }
      }
    }
  }

}
