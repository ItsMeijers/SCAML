package scaml.parser

import atto._
import Atto._
import cats.data.{NonEmptyList, State, StateT}
import cats.implicits._
import scaml.Scalar



case class YAMLState(context: Context, blockChomping: BlockChomping, directiveTable: Map[String, Directive])

object YAMLParser {

  type YamlParser[A] = StateT[Parser, YAMLState, A]

  // Indicators parsers used for clarity within the other parsers
  private val sequenceEntry: Parser[Char] = char('-')

  private val mappingKey: Parser[Char] = char('?')

  private val mappingValue: Parser[Char] = char(':')

  private val collectEntry: Parser[Char] = char(',')

  private val sequenceStart: Parser[Char] = char('[')

  private val sequenceEnd: Parser[Char] = char(']')

  private val mappingStart: Parser[Char] = char('{')

  private val mappingEnd: Parser[Char] = char('}')

  private val comment: Parser[Char] = char('#')

  private val anchor: Parser[Char] = char('&')

  private val alias: Parser[Char] = char('*')

  private val tag: Parser[Char] = char('!')

  private val literal: Parser[Char] = char('|')

  private val folded: Parser[Char] = char('>')

  private val singleQuote: Parser[Char] = char('\'')

  private val doubleQuote: Parser[Char] = char('\"')

  private val directiveChar: Parser[Char] = char('%')

  private val reserved: Parser[Char] = char('@') | char('`')

  private val indecator: Parser[Char] =
    sequenceEntry |
    mappingKey    |
    mappingValue  |
    collectEntry  |
    sequenceStart |
    sequenceEnd   |
    mappingStart  |
    mappingEnd    |
    comment       |
    anchor        |
    alias         |
    tag           |
    literal       |
    folded        |
    singleQuote   |
    doubleQuote   |
    directiveChar |
    reserved

  private val flowIndecator: Parser[Char] = collectEntry | sequenceStart | sequenceEnd | mappingStart | mappingEnd

  val breakReturnLine: Parser[Char] = char('\r') ~> char('\n')

  val breakLine: Parser[Char] = char('\n')

  val break: Parser[Char] = breakReturnLine | breakLine

  val space: Parser[Char] = spaceChar

  val tab: Parser[Char] = char('\t')

  val white: Parser[Char] = space | tab

  val wordChar: Parser[Char] = digit | hexDigit | letter | char('-')

  // URI characters for tags, as specified in RFC2396, with the addition of the “[” and “]” for presenting IPv6 addresses
  val uriChar: Parser[String] =
    (string("%"), hexDigit, hexDigit).mapN { case (p, hd, hd2) => p ++ hd.toString ++ hd2.toString}   |
      wordChar.map(_.toString)  | string("#") | string(";") | string("/") | string("?") | string(":") |
      string("@") | string("&") | string("=") | string("+") | string("$") | string(",") | string("_") |
      string(".") | string("!") | string("~") | string("*") | string("'") | string("(") | string(")") |
      string("[") | string("]")

  val tagChar: Parser[String] = (uriChar, string("!"), flowIndecator)
    .mapN { case (uc, excl, fi) => uc ++ excl ++ fi.toString }

  // TODO: escaped characters 5.7 with translation into valid Unicode characters

  def indent(n: Int): Parser[Unit] = manyN(n, space).void

  // TODO Make sure that the indentation rules are followed for block construct termination
  // [64]	s-indent(<n)	::=	s-space × m /* Where m < n */
  // [65]	s-indent(≤n)	::=	s-space × m /* Where m ≤ n */

  // Note that seperateInLine = white+ | start of line...
  def separateInLine(n: Int): Parser[Unit] = many1(white).void | startOfLine(n)

  def startOfLine(n: Int): Parser[Unit] =
    if(n == 0) ok(())
    else pos.flatMap(p => err(s"Expected start of line but indented with $n spaces at position: $p."))

  def linePrefix(n: Int, c: Context): Parser[Unit] =
    c match {
      case BlockOut => blockLinePrefix(n)
      case BlockIn  => blockLinePrefix(n)
      case FlowOut  => flowLinePrefix(n)
      case FlowIn   => flowLinePrefix(n)
      case _        => err(s"Unexpected context $c.")
    }

  def blockLinePrefix(n: Int): Parser[Unit] = indent(n)

  def flowLinePrefix(n: Int): Parser[Unit] = (indent(n) ~ opt(separateInLine(n))).void

  // TODO fix indent(n) to indent(<n)
  def emptyLine(n: Int, c: Context): Parser[Char] =
    ((linePrefix(n, c) | indent(n)) ~ break).map(_ => '\n')

  def lineTrimmed(n: Int, c: Context): Parser[String] =
    break ~> many1(emptyLine(n, c)).map(_.toList.mkString)

  def lineFolded(n: Int, c: Context): Parser[String] = lineTrimmed(n, c) | break.map(_.toString)

  def flowFolded(n: Int): Parser[String] =
    (opt(separateInLine(n)), lineFolded(n, FlowIn), flowLinePrefix(n)).mapN { case (_, s, _) => s}

  // Note that seperateInLine = white+ | start of line...
  val blockComment: Parser[Unit] =
    opt(separateInLine(0) ~ opt(comment ~ many(noneOf("\n")))) ~> (break.void | endOfInput)

  // Note that seperateInLine = white+ | start of line... therefore opt(seperateInLine)
  // TODO Fix seperateInLine for start of line
  val lineComment: Parser[Unit] = separateInLine(0) ~ opt(comment ~ many(noneOf("\n"))) ~> break.void

  // Should be blockComment | startOfLine
  val lineComments: Parser[Unit] = opt (blockComment) ~> many(lineComment).void

  def separateLines(n: Int): Parser[Unit] = (lineComments ~> flowLinePrefix(n)) | separateInLine(n)

  def separate(n: Int, c: Context): Parser[Unit] =
    c match {
      case BlockOut => separateLines(n)
      case BlockIn  => separateLines(n)
      case BlockKey => separateInLine(n)
      case FlowOut  => separateLines(n)
      case FlowIn   => separateLines(n)
      case FlowKey  => separateInLine(n)
    }

  val directiveName: Parser[String] =
     many1(noneOf("\n")).map(_.toList.mkString)

  val directiveParameter: Parser[String] =
    many1(noneOf("\n")).map(_.toList.mkString)

  val reservedDirective: Parser[Directive] =
    (directiveName, many(separateInLine(0) ~> directiveParameter))
      .mapN { case (name, parameters) => ReservedDirective(name, parameters)}

  val yamlVersion: Parser[Double] =
    (digit, char('.'), digit).mapN { case (major, d, minor) => s"$major$d$minor".toDouble }

  val yamlDirective: Parser[Directive] =
    (string("YAML") ~> separateInLine(0) ~> yamlVersion).map(YamlDirective.apply)

  val namedTagHandle: Parser[String] =
    (tag ~> many1(wordChar) <~ tag).map(_.toList.mkString)

  val secondaryTagHandle: Parser[String] = string("!!")

  val primaryTagHandle: Parser[String] = string("!")

  val tagHandle: Parser[String] = namedTagHandle | secondaryTagHandle | primaryTagHandle

  val localTagPrefix: Parser[String] =
    (string("!"), many(uriChar)).mapN { case (excl, uriChars) => excl ++ uriChars.mkString }

  val globalTagPrefix: Parser[String] =
    (tagChar, many(uriChar)).mapN { case (tc, uc) => tc ++ uc.mkString }

  val tagPrefix: Parser[String] = localTagPrefix | globalTagPrefix

  val tagDirective: Parser[Directive] =
    string("TAG") ~> (separateInLine(0) ~> tagHandle, separateInLine(0) ~> tagPrefix).mapN {
      case (handle, prefix) => TagDirective(handle, prefix)
    }

  val directive: Parser[Directive] =
    directiveChar ~> (yamlDirective | tagDirective | reservedDirective) <~ lineComments

  val verbatimTag: Parser[String] = (string("!<"), many1(uriChar), string(">"))
    .mapN { case (s, uriChars, e) => s ++ uriChars.toList.mkString ++ e }

  val shortHandTag: Parser[String] = (tagHandle, many1(tagChar))
    .mapN { case (th, tagChars) => th ++ tagChars.toList.mkString }

  /**
    * If a node has no tag property, it is assigned a non-specific tag that needs to be resolved to a specific one.
    * This non-specific tag is “!” for non-plain scalars and “?” for all other nodes. This is the only case where the
    * node style has any effect on the content information.
    * It is possible for the tag property to be explicitly set to the “!” non-specific tag. By convention, this
    * “disables” tag resolution, forcing the node to be interpreted as “tag:yaml.org,2002:seq”,
    * “tag:yaml.org,2002:map”, or “tag:yaml.org,2002:str”, according to its kind.
    */
  val nonSpecificTag: Parser[String] = string("!")

  /**
    * TODO Lift into a specific Tag product
    */
  val tagProperty: Parser[String] = verbatimTag | shortHandTag | nonSpecificTag

  // TODO fix to ns-char without flow-indecators
  val anchorChar: Parser[String] = wordChar.map(_.toString)

  val anchorName: Parser[String] = many1(anchorChar).map(_.toList.mkString)

  val anchorProperty: Parser[String] = (string("&"), anchorName).mapN { case (a, n) => a ++ n }

  case class AliasNode(reference: String)

  val aliasNode: Parser[AliasNode] = (string("*") ~> anchorName).map(AliasNode.apply)

  final case object EmptyScalar

  val emptyScalar: Parser[EmptyScalar.type] =
    many(space) ~> (collectEntry | mappingValue).map(_ => EmptyScalar)

  final case object EmptyNode

  val emptyNode: Parser[EmptyNode.type] =
    many(space) ~> (collectEntry | mappingValue).map(_ => EmptyNode)


  def properties(n: Int, c: Context): Parser[String] =
    (tagProperty, opt((separate(n, c), anchorProperty).mapN { case (_, s) => s }))
      .mapN { case (s, sOpt) => s ++ sOpt.getOrElse("") } |
    (anchorProperty, opt((separate(n, c), tagProperty).mapN { case (_, s) => s }))
      .mapN { case (s, sOpt) => s ++ sOpt.getOrElse("") }

}
