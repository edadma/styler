package xyz.hyperreal.styler

abstract class Elem
case class StringElem(s: String)      extends Elem
case class IntElem(n: Int)            extends Elem
case class ListElem(elems: Seq[Elem]) extends Elem
