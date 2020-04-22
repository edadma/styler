package xyz.hyperreal

import scala.util.parsing.input.{Position, Reader}

package object pargen {

  type Input = Reader[Char]

  def perror(msg: String): Nothing = problem(null, msg)

  def problem(pos: Position, error: String): Nothing = {
    if (pos eq null)
      println(error)
    else
      println(s"[${pos.line}, ${pos.column}]: $error\n${pos.longString}")

    sys.exit(1)
  }

}
