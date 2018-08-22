package scaml.parser

import atto._
import Atto._
import cats.implicits._

/**
  * A YAML character stream may contain several documents. Each document is completely independent from the rest.
  */
object CharacterStreamParsers {

  // Documents
  // A YAML character stream may contain several documents. Each document is completely independent from the rest.

  /**
    * [202]
    */
  val lDocumentPrefix: Parser[String] = ???

  /**
    * [203]
    */
  val cDirectivesEnd: Parser[String] = ???

  /**
    * [204]
    */
  val cDocumentEnd: Parser[String] = ???

  /**
    * [205]
    */
  val lDocumentSuffix: Parser[String] = ???

  /**
    * [206]
    */
  val cForbidden: Parser[String] = ???

  /**
    * [207]
    */
  val lBareDocument: Parser[String] = ???

  /**
    * [208]
    */
  val lExplicitDocument: Parser[String] = ???

  /**
    * [209]
    */
  val lDirectiveDocument: Parser[String] = ???

  // Streams

  /**
    * [210]
    */
  val lAnyDocument: Parser[String] = ???

  /**
    * [211]
    */
  val lYamlStream: Parser[String] = ???

}
