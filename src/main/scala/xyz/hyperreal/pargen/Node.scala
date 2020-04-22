package xyz.hyperreal.pargen

abstract class Node
case class LeafNode(typ: String, value: String)      extends Node
case class BranchNode(typ: String, nodes: Seq[Node]) extends Node
