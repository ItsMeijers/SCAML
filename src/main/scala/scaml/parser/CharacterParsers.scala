package scaml.parser

import atto._
import Atto._
import cats.implicits._

/**
  * e- A production matching no characters.
  * c- A production starting and ending with a special character.
  * b- A production matching a single line break.
  * nb- A production starting and ending with a non-break character.
  * s- A production starting and ending with a white space character.
  * ns- A production starting and ending with a non-space character.
  * l- A production matching complete line(s).
  * X-Y- A production starting with an X- character and ending with a Y- character, where X- and Y- are any of the
  * above prefixes.
  * X+, X-Y+ A production as above, with the additional property that the matched content indentation level is greater
  * than the specified n parameter.
  */
object CharacterParsers {

  // Character Set

  /**
    * [1] To ensure readability, YAML streams use only the printable subset of the Unicode character set. The allowed
    * character range explicitly excludes the C0 control block #x0-#x1F (except for TAB #x9, LF #xA, and CR #xD which
    * are allowed), DEL #x7F, the C1 control block #x80-#x9F (except for NEL #x85 which is allowed), the surrogate
    * block #xD800-#xDFFF, #xFFFE, and #xFFFF. A YAML processor must accept all Unicode characters except those
    * explicitly excluded above.
    */
  val cPrintable: Parser[String] = ???

  /**
    * [2] To ensure JSON compatibility, YAML processors must allow all non-control characters inside quoted scalars.
    */
  val nbJson: Parser[String] = ???

  // Character Encodings

  /**
    * [3]
    */
  val cByteOrderMark: Parser[String] = ???

  // Indicator Characters

  /**
    * [4] A “-” (#x2D, hyphen) denotes a block sequence entry.
    */
  val cSequenceEntry: Parser[String] = string("-")

  /**
    * [5] A “?” (#x3F, question mark) denotes a mapping key.
    */
  val cMappingKey: Parser[String] = string("?")

  /**
    * [6] A “:” (#x3A, colon) denotes a mapping value.
    */
  val cMappingValue: Parser[String] = string(":")

  /**
    * [7] A “,” (#x2C, comma) ends a flow collection entry.
    */
  val cCollectEntry: Parser[String] = string(",")

  /**
    * [8] A “[” (#x5B, left bracket) starts a flow sequence.
    */
  val cSequenceStart: Parser[String] = string("[")

  /**
    * [9] A “]” (#x5D, right bracket) ends a flow sequence.
    */
  val cSequenceEnd: Parser[String] = string("]")

  /**
    * [10] A “{” (#x7B, left brace) starts a flow mapping.
    */
  val cMappingStart: Parser[String] = string("{")

  /**
    * [11] A “}” (#x7D, right brace) ends a flow mapping.
    */
  val cMappingEnd: Parser[String] = string("}")

  /**
    * [12] An “#” (#x23, octothorpe, hash, sharp, pound, number sign) denotes a comment.
    */
  val cComment: Parser[String] = string("#")

  /**
    * [13] An “&” (#x26, ampersand) denotes a node’s anchor property.
    */
  val cAnchor: Parser[String] = string("&")

  /**
    * [14] An “*” (#x2A, asterisk) denotes an alias node.
    */
  val cAlias: Parser[String] = string("*")

  /**
    * [15] The “!” (#x21, exclamation) is heavily overloaded for specifying node tags. It is used to denote tag handles
    * used in tag directives and tag properties; to denote local tags; and as the non-specific tag for non-plain
    * scalars.
    */
  val cTag: Parser[String] = string("!")

  /**
    * [16] A “|” (7C, vertical bar) denotes a literal block scalar.
    */
  val cLiteral: Parser[String] = string("|")

  /**
    * [17] A “>” (#x3E, greater than) denotes a folded block scalar.
    */
  val cFolded: Parser[String] = string(">")

  /**
    * [18] An “'” (#x27, apostrophe, single quote) surrounds a single-quoted flow scalar.
    */
  val cSingleQuote: Parser[String] = string("'")

  /**
    * [19] A “"” (#x22, double quote) surrounds a double-quoted flow scalar.
    */
  val cDoubleQuote: Parser[String] = string("\"")

  /**
    * [20] A “%” (#x25, percent) denotes a directive line.
    */
  val cDirectiveChar: Parser[String] = string("%")

  /**
    * [21] The “@” (#x40, at) and “`” (#x60, grave accent) are reserved for future use.
    */
  val cReserved: Parser[String] = string("@") | string("`")

  /**
    * [22] Any indicator character
    */
  val cIndicator: Parser[String] =
      cSequenceEntry |
      cMappingKey    |
      cMappingValue  |
      cCollectEntry  |
      cSequenceStart |
      cSequenceEnd   |
      cMappingStart  |
      cMappingEnd    |
      cComment       |
      cAnchor        |
      cAlias         |
      cTag           |
      cLiteral       |
      cFolded        |
      cSingleQuote   |
      cDoubleQuote   |
      cDirectiveChar |
      cReserved

  /**
    * [23] The “[”, “]”, “{”, “}” and “,” indicators denote structure in flow collections. They are therefore forbidden
    * in some cases, to avoid ambiguity in several constructs. This is handled on a case-by-case basis by the relevant
    * productions.
    */
  val cFlowIndicator: Parser[String] =
    cMappingKey    |
    cSequenceEntry |
    cSequenceEnd   |
    cMappingStart  |
    cMappingEnd

  // Line Break Characters

  /**
    * YAML recognizes the following ASCII line break characters.
    * [24] #xA /* LF */
    */
  val bLineFeed: Parser[String] = ???

  /**
    * [25] #xD /* CR */
    */
  val bCarriageReturn: Parser[String] = ???

  /**
    * [26] bLineFeed or bCarriageReturn
    */
  val bChar: Parser[String] = bLineFeed | bCarriageReturn

  /**
    * [27] YAML version 1.1 did support the above non-ASCII line break characters; however, JSON does not. Hence, to
    * ensure JSON compatibility, YAML treats them as non-break characters as of version 1.2. In theory this would cause
    * incompatibility with version 1.1; in practice these characters were rarely (if ever) used. YAML 1.2 processors
    * parsing a version 1.1 document should therefore treat these line breaks as non-break characters, with an
    * appropriate warning.
    */
  val nbChar: Parser[String] = (cPrintable ~ bChar ~ cByteOrderMark)

  /**
    * [28]
    */
  val bBreak: Parser[String] = ???

  /**
    * [26]
    */
  val bAsLineFeed: Parser[String] = bBreak

  /**
    * [26]
    */
  val bNonContent: Parser[String] = bBreak

  // White Space Characters

  // Miscellaneous Characters

  // Escaped Characters

}
