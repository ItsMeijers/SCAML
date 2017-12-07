package scaml.parser

/**
  * Allows the parser to tweak their behavior according to their surrounding.
  * Within a block style context, indentation is used to delineate structure.
  * Within a flow style context, explicit indicators are used to delineate structure (natural extension of JSON).
  * Because plain scalars have no delineating indicators, they are subject to some restrictions to avoid ambiguities.
  */
sealed trait Context

/**
  * Specifies different behavior for the "-" character, when outside a block.
  */
final case object BlockOut extends Context

/**
  * Specifies different behavior for the "-" character, when inside a block.
  */
final case object BlockIn extends Context

/**
  * Indicates restrictions on a plain scalar when appearing as implicit keys directly inside a block mapping.
  */
final case object BlockKey extends Context

/**
  * Indicates restrictions on a plain scalar when appearing as values outside a flow collection.
  */
final case object FlowOut extends Context

/**
  * Indicates restrictions on a plain scalar when appearing as values inside a flow collection.
  */
final case object FlowIn extends Context

/**
  * Indicates restrictions on a plain scalar when appearing as implicit keys inside a flow mapping.
  */
final case object FlowKey extends Context