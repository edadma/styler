package xyz.hyperreal.pargen

object Main extends App {

  val input =
    """
      |expression = term { ("+"| "-") term }.
      |term = number | ident.
      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)

}
