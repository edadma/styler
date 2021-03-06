package xyz.hyperreal.styler

import java.io.PrintStream

import scala.collection.mutable
import scala.util.parsing.input.Position

object Interpreter {

  private val reserved = Set("print", "printSeq", "printIndent", "printDedent", "printSpace")

  val spaces = 2

  def apply(ast: FAST, elem: Elem, out: PrintStream): Unit = {
    val col      = VariableDeclaration(null, "col", LiteralExpression(null, 0))
    val declsMap = mutable.HashMap[String, DeclarationFAST]("col" -> col)
    var level    = 0
    var nl       = true

    def escape(s: String) = s.replace("\n", "\\n")

    def outs(a: Any): Unit = a.toString foreach outc

    def outc(c: Char): Unit =
      c match {
        case '\n' =>
          out.print('\n')
          nl = true
          col.value = 0
        case _ =>
          if (nl) {
            out.print(" " * spaces * level)
            nl = false
          }

          out.print(c)
          col.value = col.value.asInstanceOf[Int] + 1
      }

    def indent(): Unit = level += 1

    def dedent(): Unit = level -= 1

    def eval(expr: ExpressionFAST, locals: Map[String, Any] = Map()): Any =
      expr match {
        case LiteralExpression(pos, literal: String) => StringElem(literal)
        case LiteralExpression(pos, literal: Int)    => IntElem(literal)
        case VariableExpression(pos, name) =>
          declsMap get name match {
            case Some(VariableDeclaration(_, _, _, value)) => value
            case Some(f: FunctionDeclaration)              => f
            case Some(_)                                   => problem(pos, "not a variable")
            case None =>
              locals get name match {
                case Some(x) => x
                case None    => problem(pos, "variable not found")
              }
          }
        case b: BlockExpression => b
        case _                  =>
      }

    def execute(stmt: StatementFAST, locals: Map[String, Any]): Unit =
      stmt match {
        case ApplyStatement(pos, func, args) =>
          val argvals = args map (a => eval(a, locals))

          (func, argvals) match {
            case ("printSpace", List(n: Int))   => outs(" " * n)
            case ("print", List(StringElem(s))) => outs(s)
            case ("print", a)                   => print(a)
            case ("printIndent", List(StringElem(s))) =>
              outs(s)
              outs('\n')
              indent()
            case ("printDedent", List(StringElem(s))) =>
              outs('\n')
              dedent()
              outs(s)
            case ("printSeq", List(ListElem(branches), sep)) =>
              if (branches nonEmpty) {
                branches.init foreach { b =>
                  printElem(pos, b)

                  sep match {
                    case StringElem(s)             => outs(s)
                    case BlockExpression(_, stmts) => stmts foreach (execute(_, locals))
                  }
                }

                printElem(pos, branches.last)
              }
            case _ => call(pos, func, argvals)
          }
        case BlockStatement(stmts) => stmts foreach (execute(_, locals))
        case AssignmentStatement(pos, name, epos, expr) =>
          declsMap get name match {
            case Some(v: VariableDeclaration) => v.value = eval(expr, locals)
            case Some(_)                      => problem(pos, "not a variable")
            case None                         => problem(pos, "not a global variable")
          }
      }

    def call(pos: Position, func: String, args: Seq[Any]): Unit = {
      declsMap get func match {
        case Some(NativeDeclaration(name, func)) => func(args)
        case Some(FunctionDeclaration(_, _, cases)) =>
          val arg = if (args.length == 1) args.head else args

          def addvar(pos: Position, name: String, value: Any, vars: Map[String, Any]) =
            vars get name match {
              case Some(_) => problem(pos, "pattern variable already used")
              case None    => Some(vars + (name -> value))
            }

          def unify(pat: PatternFAST, value: Any, vars: Map[String, Any]): Option[Map[String, Any]] =
            (pat, value) match {
              case (AnyPattern, _) => Some(vars)
              case (StringPattern(pos, s), StringElem(value)) =>
                if (s == value)
                  Some(vars)
                else
                  None
              case (VariablePattern(pos, name), value) => addvar(pos, name, value, vars)
              case (ListPattern(pos, branches), ListElem(ebranches)) =>
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

                  unifyList(branches zip ebranches, vars)
                } else
                  None
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
              case Nil => problem(pos, s"none of the cases matched: $arg")
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

    def declare(decl: DeclarationFAST): Unit =
      if (reserved(decl.name))
        problem(decl.pos, "name is reserved")
      else
        declsMap get decl.name match {
          case Some(_) => problem(decl.pos, "name already used")
          case None    => declsMap(decl.name) = decl
        }

    def printElem(pos: Position, elem: Elem): Unit = call(pos, "printElem", Seq(elem))

    def apply(ast: FAST): Unit =
      ast match {
        case FormatFAST(decls) =>
          decls foreach apply
          decls foreach {
            case v @ VariableDeclaration(_, _, init, _) => v.value = eval(init)
            case _                                      =>
          }
          printElem(null, elem)
        case decl: DeclarationFAST => declare(decl)
      }

    apply(ast)
  }

}
