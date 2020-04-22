package xyz.hyperreal.pargen

object Main extends App {

  val input =
    """
      |expression = term { ("+"| "-") term } <@lassoc>.
      |term = number | ident.
      |""".stripMargin
  val ast = SyntaxNotationParser(input)

  println(ast)

}
