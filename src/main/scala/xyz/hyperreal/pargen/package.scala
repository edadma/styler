package xyz.hyperreal

import scala.util.parsing.input.Position

package object pargen {

  def perror(msg: String): Nothing = problem(null, msg)

  def problem(pos: Position, error: String): Nothing = {
    if (pos eq null)
      println(error)
    else
      println(s"[${pos.line}, ${pos.column}]: $error\n${pos.longString}")

    sys.exit(1)
  }

}
