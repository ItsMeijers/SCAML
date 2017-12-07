package scaml

sealed trait Tag

/**
  * A local tag is as the name implies local to the program and denoted as: '!'
  * @param name the name of the tag for instance: !!invoice
  * @param kind can be either Scalar, Mapping or Sequence
  */
final case class LocalTag(name: String, kind: TagKind) extends Tag

/**
  * The kind that a Tag refers to
  */
sealed trait TagKind
final case object ScalarTag extends TagKind
final case object MappingTag extends TagKind
final case object SequenceTag extends TagKind

/**
  * Global tags are part of the failsafe schema of YAML and denoted as: '!!'
  * Each tag has an associated uri, kind and name however these values are not needed
  * since they are made explicit through their types
  */
sealed trait GlobalTag extends Tag
final case object GlobalMappingTag extends GlobalTag
final case object GlobalSequenceTag extends GlobalTag
final case object GlobalStringTag extends GlobalTag
final case object GlobalNullTag extends GlobalTag
final case object GlobalBooleanTag extends GlobalTag
final case object GlobalIntegerTag extends GlobalTag
final case object GlobalFloatTag extends GlobalTag
