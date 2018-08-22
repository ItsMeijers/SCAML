package scaml.parser

import cats.Show

sealed trait YamlParseError

final case class WrongContextStateError(context: Context) extends YamlParseError

final case class YamlIndentationError(identation: Int) extends YamlParseError

object YamlParseError {

  implicit val YamlParseErrorShowInstance: Show[YamlParseError] = new Show[YamlParseError] {
    override def show(yamlParseError: YamlParseError): String =
      yamlParseError match {
        case WrongContextStateError(context)  => s"Line prefix not allowed in wrong context: `$context`."
        case YamlIndentationError(indentation) => s"Indentation error: $indentation"
      }
  }

}