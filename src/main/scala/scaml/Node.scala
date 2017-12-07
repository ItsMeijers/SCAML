package scaml

sealed trait Node
final case class Scalar(tag: Tag, content: String) extends Node
final case class Sequence(tag: Tag, content: Vector[Node]) extends Node
final case class Mapping(tag: Tag, content: Map[Node, Node]) extends Node
