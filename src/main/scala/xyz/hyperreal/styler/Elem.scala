package xyz.hyperreal.styler

abstract class Elem
case class LiftElem(n: Elem)                         extends Elem
case class AddElem(n: Elem)                          extends Elem
case class LiteralElem(literal: String)              extends Elem
case class LeafElem(typ: String, value: String)      extends Elem
case class BranchElem(typ: String, nodes: Seq[Elem]) extends Elem
