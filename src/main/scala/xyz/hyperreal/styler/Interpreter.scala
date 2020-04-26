package xyz.hyperreal.styler

import scala.collection.mutable

object Interpreter {

  def apply(ast: FAST, elem: Elem) = {
    val declsMap = new mutable.HashMap[String, DeclarationFAST]

    def eval(expr: ExpressionFAST) =
      expr match {
        case LiteralExpression(literal) => literal
        case VariableExpression(pos, name) =>
          declsMap get name match {
            case Some(VariableDeclaration(_, _, _, value)) => value
            case None                                      => problem(pos, "variable not found")
          }
        case _ =>
      }

    ast match {
      case FormatFAST(decls) =>
        decls foreach (apply(_, elem))
        decls foreach {
          case v @ VariableDeclaration(_, _, init, _) => v.value = eval(init)
          case _                                      =>
        }
      case decl @ VariableDeclaration(pos, name, value, _) =>
        declsMap get name match {
          case Some(_) => problem(pos, "name already used")
          case None    => declsMap(name) = decl
        }
    }
  }

}
