package scaml

import scaml.Presentation.Presentation
import scaml.Representation.Representation

object Serialization {

  type Serialization = Any

  val present: Serialization => Presentation = ???

  val compose: Serialization => Representation = ???

}
