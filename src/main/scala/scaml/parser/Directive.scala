package scaml.parser

/**
  * Directives are instructions to the YAML processor.
  */
sealed trait Directive

/**
  * Specifies the YAML version at the top of a document.
  * There can only be one YAML directive per document.
  */
final case class YamlDirective(version: Double) extends Directive

/**
  * The “TAG” directive establishes a tag shorthand notation for specifying node tags. Each “TAG” directive
  * associates a handle with a prefix. This allows for compact and readable tag notation.
  */
final case class TagDirective(handle: String, prefix: String) extends Directive

/**
  * All other tags are reserved for future YAML versions.
  */
final case class ReservedDirective(name: String, parameters: List[String]) extends Directive