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

    def eval(expr: ExpressionFAST, locals: Map[String, Any] = Map()): Any =
      expr match {
        case LiteralExpression(pos, literal) => literal
        case VariableExpression(pos, name) =>
          declsMap get name match {
            case Some(VariableDeclaration(_, _, _, value)) => value
            case Some(_)                                   => problem(pos, "not a variable")
            case None =>
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
            def execute(stmt: StatementFAST, locals: Map[String, Any]): Unit =
              stmt match {
                case ApplyStatement(pos, func, args) => call(pos, func, args map (a => eval(a, locals)))
                case BlockStatement(stmts)           => stmts foreach (execute(_, locals))
                case _                               =>
              }

            val arg = if (args.length == 1) args.head else args

            def addvar(pos: Position, name: String, value: Any, vars: Map[String, Any]) =
              vars get name match {
                case Some(_) => problem(pos, "pattern variable already used")
                case None    => Some(vars + (name -> value))
              }

            def unify(pat: PatternFAST, value: Any, vars: Map[String, Any]): Option[Map[String, Any]] =
              (pat, value) match {
                case (AnyPattern, _) => Some(vars)
                case (StringPattern(pos, s), value: String) =>
                  if (s == value)
                    Some(vars)
                  else
                    None
                case (VariablePattern(pos, name), value) => addvar(pos, name, value, vars)
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
                        @scala.annotation.tailrec
                        def unifyList(l: Seq[(PatternFAST, Any)], vars: Map[String, Any]): Option[Map[String, Any]] =
                          l match {
                            case Nil => Some(vars)
                            case (p, e) :: tail =>
                              unify(p, e, vars) match {
                                case None    => None
                                case Some(m) => unifyList(tail, m)
                              }
                          }

                        unifyList(branches zip ebranches, m)
                      } else
                        None
                  }
                case (AlternatesPattern(alts), _) =>
                  @scala.annotation.tailrec
                  def unifyAlts(alts: Seq[PatternFAST]): Option[Map[String, Any]] =
                    alts match {
                      case Nil => None
                      case p :: tail =>
                        unify(p, value, vars) match {
                          case None => unifyAlts(tail)
                          case r    => r
                        }
                    }

                  unifyAlts(alts)
                case (NamedPattern(pos, name, pat), _) =>
                  unify(pat, value, vars) match {
                    case Some(m) => addvar(pos, name, value, m)
                    case None    => None
                  }
                case _ => None
              }

            @scala.annotation.tailrec
            def matchCases(cases: Seq[(PatternFAST, StatementFAST)]): Unit =
              cases match {
                case Nil => problem(pos, "none of the cases matched")
                case (pat, stmt) :: tail =>
                  unify(pat, arg, Map()) match {
                    case Some(m) => execute(stmt, m)
                    case None    => matchCases(tail)
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
