package xyz.hyperreal.styler

import scala.collection.mutable
import scala.util.parsing.input.Position

object Interpreter {

  private val builtins =
    List[(String, Seq[Any] => Unit)](
      "print" -> (args => print(args mkString ", "))
    )

  def apply(ast: FAST, elem: Elem): Unit = {
    val builtinDecls =
      builtins map {
        case (n, f) => (n, NativeDeclaration(n, f))
      }
    val declsMap = mutable.HashMap[String, DeclarationFAST](builtinDecls: _*)

    def eval(expr: ExpressionFAST, locals: mutable.HashMap[String, Any] = null): Any =
      expr match {
        case LiteralExpression(pos, literal) => literal
        case VariableExpression(pos, name) =>
          declsMap get name match {
            case Some(VariableDeclaration(_, _, _, value)) => value
            case None =>
              if (locals eq null)
                problem(pos, "variable not found")
              else
                locals get name match {
                  case Some(x) => x
                  case None    => problem(pos, "variable not found")
                }
          }
        case _ =>
      }

    def apply(ast: FAST): Unit = {
      def call(pos: Position, func: String, args: Seq[Any]): Unit = {
        declsMap get func match {
          case Some(NativeDeclaration(name, func)) => func(args)
          case Some(FunctionDeclaration(_, _, cases)) =>
            val locals = new mutable.HashMap[String, Any]

            def execute(stmt: StatementFAST): Unit =
              stmt match {
                case ApplyStatement(pos, func, args) => call(pos, func, args map (a => eval(a, locals)))
                case BlockStatement(stmts)           => stmts foreach execute
                case _                               =>
              }

            val arg = if (args.length == 1) args.head else args

            def unify(pat: PatternFAST, value: Any, vars: Map[String, Any]): Option[Map[String, Any]] =
              (pat, value) match {
                case (StringPattern(pos, s), value: String) =>
                  if (s == value)
                    Some(vars)
                  else
                    None
                case (VariablePattern(pos, name), value) =>
                  locals get name match {
                    case Some(_) => problem(pos, "local variable already used")
                    case None    => Some(vars + (name -> value))
                  }
                case (LeafPattern(pos, typ, value), LeafElem(etyp, evalue)) =>
                  unify(typ, etyp, vars) match {
                    case None    => None
                    case Some(m) => unify(value, evalue, m)
                  }
                case (BranchPattern(pos, typ, branches), BranchElem(etyp, ebranches)) =>
                  unify(typ, etyp, vars) match {
                    case None => None
                    case Some(m) =>
                      if (branches.length == ebranches.length) {
                        def unifyList(l: Seq[(PatternFAST, Any)], vars: Map[String, Any]): Option[Map[String, Any]] =
                          l match {
                            case Nil => Some(vars)
                            case (p, e) :: tail =>
                              unify(p, e, vars) match {
                                case None => unifyList(tail, vars)
                                case r    => r
                              }
                          }

                        unifyList(branches zip ebranches, m)
                      } else
                        None
                  }
                case (AlternatesPattern(alts), _) => alts.exists(p => unify(p, value))
                case _                            => false
              }

            def matchCases(cases: Seq[(PatternFAST, StatementFAST)]): Unit =
              cases match {
                case Nil => problem(pos, "none of the cases matched")
                case (pat, stmt) :: tail =>
                  if (unify(pat, arg))
                    execute(stmt)
                  else {
                    locals.clear
                    matchCases(tail)
                  }
              }

            matchCases(cases)
          case _ => problem(pos, "function not declared")
        }
      }

      def declare(decl: DeclarationFAST): Unit = {
        declsMap get decl.name match {
          case Some(_) => problem(decl.pos, "name already used")
          case None    => declsMap(decl.name) = decl
        }
      }

      ast match {
        case FormatFAST(decls) =>
          decls foreach apply
          decls foreach {
            case v @ VariableDeclaration(_, _, init, _) => v.value = eval(init)
            case _                                      =>
          }
          call(null, "printElem", Seq(elem))
        case decl: DeclarationFAST => declare(decl)
      }
    }

    apply(ast)
  }

}
