package scaml.parser

import atto._
import Atto._
import cats.implicits._
import scala.collection.immutable.NumericRange

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

  def charRanges(start: Int, end: Int): Parser[Char] =
    charRange(NumericRange.inclusive(start.asInstanceOf[Char], end.asInstanceOf[Char], 1))

  def charCode(code: Int): Parser[Char] = char(code.asInstanceOf[Char])

  /**
    * [1] To ensure readability, YAML streams use only the printable subset of the Unicode character set. The allowed
    * character range explicitly excludes the C0 control block #x0-#x1F (except for TAB #x9, LF #xA, and CR #xD which
    * are allowed), DEL #x7F, the C1 control block #x80-#x9F (except for NEL #x85 which is allowed), the surrogate
    * block #xD800-#xDFFF, #xFFFE, and #xFFFF. A YAML processor must accept all Unicode characters except those
    * explicitly excluded above.
    */
  val cPrintable: Parser[Char] =
    charCode(0x9)  |
    charCode(0xA)  |
    charCode(0xD)  |
    charRanges(0x20, 0x7E)        |
    charCode(0x85) |
    charRanges(0xA0, 0xD7FF)      |
    charRanges(0x10000, 0x10FFFF)

  /**
    * [2] To ensure JSON compatibility, YAML processors must allow all non-control characters inside quoted scalars.
    */
  val nbJson: Parser[Char] = charCode(0x9) |
    charRange(NumericRange.inclusive(0x20.asInstanceOf[Char], 0x10FFFF.asInstanceOf[Char], 1))

  // Character Encodings

  /**
    * [3]
    */
  val cByteOrderMark: Parser[Char] = charCode(0xFEFF)

  // Indicator Characters

  /**
    * [4] A “-” (#x2D, hyphen) denotes a block sequence entry.
    */
  val cSequenceEntry: Parser[Char] = char('-')

  /**
    * [5] A “?” (#x3F, question mark) denotes a mapping key.
    */
  val cMappingKey: Parser[Char] = char('?')

  /**
    * [6] A “:” (#x3A, colon) denotes a mapping value.
    */
  val cMappingValue: Parser[Char] = char(':')

  /**
    * [7] A “,” (#x2C, comma) ends a flow collection entry.
    */
  val cCollectEntry: Parser[Char] = char(',')

  /**
    * [8] A “[” (#x5B, left bracket) starts a flow sequence.
    */
  val cSequenceStart: Parser[Char] = char('[')

  /**
    * [9] A “]” (#x5D, right bracket) ends a flow sequence.
    */
  val cSequenceEnd: Parser[Char] = char(']')

  /**
    * [10] A “{” (#x7B, left brace) starts a flow mapping.
    */
  val cMappingStart: Parser[Char] = char('{')

  /**
    * [11] A “}” (#x7D, right brace) ends a flow mapping.
    */
  val cMappingEnd: Parser[Char] = char('}')

  /**
    * [12] An “#” (#x23, octothorpe, hash, sharp, pound, number sign) denotes a comment.
    */
  val cComment: Parser[Char] = char('#')

  /**
    * [13] An “&” (#x26, ampersand) denotes a node’s anchor property.
    */
  val cAnchor: Parser[Char] = char('&')

  /**
    * [14] An “*” (#x2A, asterisk) denotes an alias node.
    */
  val cAlias: Parser[Char] = char('*')

  /**
    * [15] The “!” (#x21, exclamation) is heavily overloaded for specifying node tags. It is used to denote tag handles
    * used in tag directives and tag properties; to denote local tags; and as the non-specific tag for non-plain
    * scalars.
    */
  val cTag: Parser[Char] = char('!')

  /**
    * [16] A “|” (7C, vertical bar) denotes a literal block scalar.
    */
  val cLiteral: Parser[Char] = char('|')

  /**
    * [17] A “>” (#x3E, greater than) denotes a folded block scalar.
    */
  val cFolded: Parser[Char] = char('>')

  /**
    * [18] An “'” (#x27, apostrophe, single quote) surrounds a single-quoted flow scalar.
    */
  val cSingleQuote: Parser[Char] = char(''')

  /**
    * [19] A “"” (#x22, double quote) surrounds a double-quoted flow scalar.
    */
  val cDoubleQuote: Parser[Char] = char('\"')

  /**
    * [20] A “%” (#x25, percent) denotes a directive line.
    */
  val cDirectiveChar: Parser[Char] = char('%')

  /**
    * [21] The “@” (#x40, at) and “`” (#x60, grave accent) are reserved for future use.
    */
  val cReserved: Parser[Char] = char('@') | char('`')

  /**
    * [22] Any indicator character
    */
  val cIndicator: Parser[Char] =
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
  val cFlowIndicator: Parser[Char] =
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
  val bLineFeed: Parser[Char] = charCode(0xA)

  /**
    * [25] #xD /* CR */
    */
  val bCarriageReturn: Parser[Char] = charCode(0xD)

  /**
    * [26] bLineFeed or bCarriageReturn
    */
  val bChar: Parser[Char] = bLineFeed | bCarriageReturn

  def except[A](parser: Parser[A], msg: String)(exceptions: Parser[A]*): Parser[A] =
    exceptions.reduce(_ | _).flatMap(_ => err[A](msg)) | parser

  /**
    * [27] YAML version 1.1 did support the above non-ASCII line break characters; however, JSON does not. Hence, to
    * ensure JSON compatibility, YAML treats them as non-break characters as of version 1.2. In theory this would cause
    * incompatibility with version 1.1; in practice these characters were rarely (if ever) used. YAML 1.2 processors
    * parsing a version 1.1 document should therefore treat these line breaks as non-break characters, with an
    * appropriate warning.
    */
  val nbChar: Parser[Char] =
    except(cPrintable, "b-char or c-byte-order-mark not allowed in nb-char")(bChar, cByteOrderMark)

  /**
    * [28] Line breaks are interpreted differently by different systems, and have several widely used formats.
    * Needs to be a string due to the DOS and Windows compatability.
    */
  val bBreak: Parser[String] =
    (bCarriageReturn ~ bLineFeed).map { case (x: Char, y: Char) => s"$x$y" } |
    bCarriageReturn.map(_.toString)               |
    bLineFeed.map(_.toString)

  /**
    * [29] Line breaks inside scalar content must be normalized by the YAML processor. Each such line break must be
    * parsed into a single line feed character. The original line break format is a presentation detail and must not
    * be used to convey content information.
    */
  val bAsLineFeed: Parser[String] = bBreak

  /**
    * [30] Outside scalar content, YAML allows any line break to be used to terminate lines.
    */
  val bNonContent: Parser[String] = bBreak

  // White Space Characters

  /**
    * [31] YAML recognizes spaces.
    */
  val sSpace: Parser[Char] = charCode(0x20)

  /**
    * [32] YAML recognizes tabs.
    */
  val sTab: Parser[Char] = charCode(0x9)

  /**
    * [33] YAML recognizes two white space characters: space and tab.
    */
  val sWhite: Parser[Char] = sSpace | sTab

  /**
    * [34] The rest of the (printable) non-break characters are considered to be non-space characters.
    */
  val nsChar: Parser[Char] = except(nbChar, "s-white not allowed.")(sWhite)

  // Miscellaneous Characters: The YAML syntax productions make use of the following additional character classes

  /**
    * [35] A decimal digit for numbers.
    */
  val nsDecDigit: Parser[Char] = charRanges(0x30, 0x39)

  /**
    * [36] A hexadecimal digit for escape sequences.
    */
  val nsHexDigit: Parser[Char] = nsDecDigit | charRanges(0x41, 0x46) | charRanges(0x61, 0x66)

  /**
    * [37] ASCII letter (alphabetic) characters.
    */
  val nsASCIILetter: Parser[Char] = charRanges(0x41, 0x5A) | charRanges(0x61, 0x7A)

  /**
    * [38] Word (alphanumeric) characters for identifiers.
    */
  val nsWordChar: Parser[Char] = nsDecDigit | nsASCIILetter | char('-')

  /**
    * [39] URI characters for tags, as specified in RFC2396, with the addition of the “[” and “]” for presenting IPv6
    * addresses as proposed in RFC2732.
    */
  val nsUriChar: Parser[String] =
    (char('%'), nsHexDigit, nsHexDigit).mapN((a, b, c) => s"$a$b$c") |
    nsWordChar.map(_.toString) |
    string("#") |
    string(";") |
    string("/") |
    string("?") |
    string(":") |
    string("@") |
    string("&") |
    string("=") |
    string("+") |
    string("$") |
    string(",") |
    string("_") |
    string(".") |
    string("!") |
    string("~") |
    string("*") |
    string("'") |
    string("(") |
    string(")") |
    string("[") |
    string("]")

  /**
    * [40] The “!” character is used to indicate the end of a named tag handle; hence its use in tag shorthands is
    * restricted. In addition, such shorthands must not contain the “[”, “]”, “{”, “}” and “,” characters.
    */
  val nsTagChar: Parser[String] =
    except(nsUriChar, "! and c-flow-indicator not allowed.")(string("!"), cFlowIndicator.map(_.toString))

  // Escaped Characters: YAML escape sequences are a superset of C’s escape sequences.

  /**
    * [41] Note that escape sequences are only interpreted in double-quoted scalars. In all other scalar styles, the “\”
    * character has no special meaning and non-printable characters are not available.
    */
  def stringChar(char: Char): Parser[String] = string(char.toString)

  def stringASCI(code: Int): Parser[String] = stringChar(code.asInstanceOf[Char])

  val cEscape: Parser[String] = stringChar('\\')

  /**
    * [42] Escaped ASCII null (#x0) character.
    */
  val nsEscNull: Parser[String] = stringASCI(0x0)

  /**
    * [43] Escaped ASCII bell (#x7) character.
    */
  val nsEscBell: Parser[String] = stringASCI(0x7)

  /**
    * [44] Escaped ASCII backspace (#x8) character.
    */
  val nsEscBackspace: Parser[String] = stringASCI(0x8)

  /**
    * [45] Escaped ASCII horizontal tab (#x9) character. This is useful at the start or the end of a line to force a
    * leading or trailing tab to become part of the content.
    */
  val nsEscHorizontalTab: Parser[String] = stringASCI(0x9)

  /**
    * [46] Escaped ASCII line feed (#xA) character.
    */
  val nsEscLineFeed: Parser[String] = stringASCI(0xA)

  /**
    * [47] Escaped ASCII vertical tab (#xB) character.
    */
  val nsEscVerticalTab: Parser[String] = stringASCI(0xB)

  /**
    * [48] 	Escaped ASCII form feed (#xC) character.
    */
  val nsEscFormFeed: Parser[String] = stringASCI(0xC)

  /**
    * [49] 	Escaped ASCII carriage return (#xD) character.
    */
  val nsEscCarriageReturn: Parser[String] = stringASCI(0xD)

  /**
    * [50] 	Escaped ASCII escape (#x1B) character.
    */
  val nsEscEscape: Parser[String] = stringASCI(0x1B)

  /**
    * [51] Escaped ASCII space (#x20) character. This is useful at the start or the end of a line to force a leading or
    * trailing space to become part of the content.
    */
  val nsEscSpace: Parser[String] = stringASCI(0x20)

  /**
    * [52] Escaped ASCII double quote (#x22).
    */
  val nsEscDoubleQuote: Parser[String] = stringASCI(0x22)

  /**
    * [53] 	Escaped ASCII slash (#x2F), for JSON compatibility.
    */
  val nsEscSlash: Parser[String] = stringASCI(0x2F)

  /**
    * [54] 	Escaped ASCII back slash (#x5C).
    */
  val nsEscBackslash: Parser[String] = stringASCI(0x5C)

  /**
    * [55] 	Escaped Unicode next line (#x85) character.
    */
  val nsEscNextLine: Parser[String] = stringASCI(0x85)

  /**
    * [56] Escaped Unicode non-breaking space (#xA0) character.
    */
  val nsEscNonBreakingSpace: Parser[String] = stringASCI(0xA0)

  /**
    * [57] Escaped Unicode line separator (#x2028) character.
    */
  val nsEscLineSeparator: Parser[String] = stringASCI(0x2048)

  /**
    * [58] 	Escaped Unicode paragraph separator (#x2029) character.
    */
  val nsEscParagraphSeparator: Parser[String] = stringASCI(0x2029)

  /**
    * [59]Escaped 8-bit Unicode character.
    */
  val nsEsc8Bit: Parser[String] = (string("x"), manyN(2, nsHexDigit)).mapN((s, lc) => s + lc.mkString)

  /**
    * [60]Escaped 16-bit Unicode character.
    */
  val nsEsc16Bit: Parser[String] = (string("u"), manyN(4, nsHexDigit)).mapN((s, lc) => s + lc.mkString)

  /**
    * [61] Escaped 32-bit Unicode character.
    */
  val nsEsc32Bit: Parser[String] = (string("U"), manyN(8, nsHexDigit)).mapN((s, lc) => s + lc.mkString)

  /**
    * [62] Any escaped character.
    */
  val cNsEscChar: Parser[String] = (cEscape,
      nsEscNull           | nsEscBell                | nsEscBackspace
    | nsEscHorizontalTab  | nsEscLineFeed            | nsEscVerticalTab
    | nsEscFormFeed       | nsEscCarriageReturn      | nsEscEscape
    | nsEscSpace          | nsEscDoubleQuote         | nsEscSlash
    | nsEscBackslash      | nsEscNextLine            | nsEscNonBreakingSpace
    | nsEscLineSeparator  | nsEscParagraphSeparator  | nsEsc8Bit
    | nsEsc16Bit          | nsEsc32Bit).mapN(_ + _)
}
