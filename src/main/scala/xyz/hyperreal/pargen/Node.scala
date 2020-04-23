package xyz.hyperreal.pargen

abstract class Node
case class LiftNode(n: Node)                         extends Node
case class LiteralNode(literal: String)              extends Node
case class LeafNode(typ: String, value: String)      extends Node
case class BranchNode(typ: String, nodes: Seq[Node]) extends Node
