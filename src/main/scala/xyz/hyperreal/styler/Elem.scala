package xyz.hyperreal.styler

abstract class Elem
case class LiftElem(n: Elem)                            extends Elem
case class AddElem(n: Elem)                             extends Elem
case class StringElem(s: String)                        extends Elem
case class IntElem(n: Int)                              extends Elem
case class LeafElem(typ: String, value: String)         extends Elem
case class BranchElem(typ: String, branches: Seq[Elem]) extends Elem
