package scaml

import scaml.Serialization.Serialization

object Representation {

  type Representation = Any

  def represent[A]: A => Representation = ???

  def construct[A]: Representation => A = ???

  val serialize: Representation => Serialization = ???

}
