package scaml.parser

import atto._
import Atto._
import cats.implicits._

/**
  * YAML’s flow styles can be thought of as the natural extension of JSON to cover folding long content lines for
  * readability, tagging nodes to control construction of native data structures, and using anchors and aliases to
  * reuse constructed object instances.
  */
object FlowStyleParsers {

  // Alias Nodes

  /**
    * [104] An alias node is denoted by the “*” indicator. The alias refers to the most recent preceding node having the
    * same anchor. It is an error for an alias node to use an anchor that does not previously occur in the document. It
    * is not an error to specify an anchor that is not used by any alias node.
    */
  val cNsAliasNode: Parser[String] = ???

  // Empty Nodes

  /**
    * [105] YAML allows the node content to be omitted in many cases. Nodes with empty content are interpreted as if
    * they were plain scalars with an empty value. Such nodes are commonly resolved to a “null” value.
    */
  val eScalar: Parser[String] = ???

  /**
    * [106] Both the node’s properties and node content are optional. This allows for a completely empty node.
    * Completely empty nodes are only valid when following some explicit indication for their existence.
    */
  val eNode: Parser[String] = ???

  // Flow Scalar Styles: YAML provides three flow scalar styles: double-quoted, single-quoted and plain (unquoted).
  // Each provides a different trade-off between readability and expressive power.

  /**
    * [107] The double-quoted style is specified by surrounding “"” indicators. This is the only style capable of
    * expressing arbitrary strings, by using “\” escape sequences. This comes at the cost of having to escape the “\”
    * and “"” characters.
    */
  val nbDoubleChar: Parser[String] = ???

  /**
    * [108]
    */
  val nsDoubleChar: Parser[String] = ???

  /**
    * [109] Double-quoted scalars are restricted to a single line when contained inside an implicit key.
    */
  def cDoubleQuoted(n: Int, context: Context): Parser[String] = ???

  /**
    * [110]
    */
  def nbDoubleText(n: Int, context: Context): Parser[String] = ???

  /**
    * [111]
    */
  val nbDoubleOneLine: Parser[String] = ???

  /**
    * [112]
    */
  def sDoubleEscaped(n: Int): Parser[String] = ???

  /**
    * [113]
    */
  def sDoubleBreak(n: Int): Parser[String] = ???

  /**
    * [114]
    */
  val nbNsDoubleInLine: Parser[String] = ???

  /**
    * [115]
    */
  def sDoubleNExtLine(n: Int): Parser[String] = ???

  /**
    * [116]
    */
  def nbDoubleMultiLine(n: Int): Parser[String] = ???

  /**
    * [117]
    */
  val cQuotedQuote: Parser[String] = ???

  /**
    * [118]
    */
  val nbSingleChar: Parser[String] = ???

  /**
    * [119]
    */
  val nsSingleChar: Parser[String] = ???

  /**
    * [120]
    */
  def cSingleQuoted(n: Int, context: Context): Parser[String] = ???

  /**
    * [121]
    */
  def nbSingleText(n: Int, context: Context): Parser[String] = ???

  /**
    * [122]
    */
  val nbSingleOneLine: Parser[String] = ???

  /**
    * [123]
    */
  val nbNsSingleInLine: Parser[String] = ???

  /**
    * [124]
    */
  def sSingleNextLine(n: Int): Parser[String] = ???

  /**
    * [125]
    */
  def nbSingleMultiLine(n: Int): Parser[String] = ???

  /**
    * [126]
    */
  def nsPlainFirst(context: Context): Parser[String] = ???

  /**
    * [127]
    */
  def nsPlainSafe(context: Context): Parser[String] = ???

  /**
    * [128]
    */
  val nsPlainSafeOut: Parser[String] = ???

  /**
    * [129]
    */
  val nsPlainSafeIn: Parser[String] = ???

  /**
    * [130]
    */
  def nsPlainChar(context: Context) : Parser[String] = ???

  /**
    * [131]
    */
  def nsPlain(n: Int, context: Context) : Parser[String] = ???

  /**
    * [132]
    */
  def nbNsPlainInLine(context: Context): Parser[String] = ???

  /**
    * [133]
    */
  def nsPlainOneLine(context: Context): Parser[String] = ???

  /**
    * [134]
    */
  def sNsPlainNextLine(n: Int, context: Context): Parser[String] = ???

  /**
    * [135]
    */
  def nsPlainMultiLine(n: Int, context: Context): Parser[String] = ???

  // Flow Collection Styles
  /**
    * [136]
    */
  def inFlow(context: Context): Parser[String] = ???

  /**
    * [137]
    */
  def cFlowSequence(n: Int, context: Context): Parser[String] = ???

  /**
    * [138]
    */
  def nsSFlowSeqEntries(n: Int, context: Context): Parser[String] = ???

  /**
    * [139]
    */
  def nsFlowSeqEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [140]
    */
  def cFlowMapping(n: Int, context: Context): Parser[String] = ???

  /**
    * [141]
    */
  def nsSFlowMapEntries(n: Int, context: Context): Parser[String] = ???

  /**
    * [142]
    */
  def nsFlowMapEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [143]
    */
  def nsFlowMapExplicitEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [144]
    */
  def nsFlowMapImplicitEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [145]
    */
  def nsFlowMapYamlKeyEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [146]
    */
  def cNsFlowMapEmptyKeyEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [147]
    */
  def cNsFlowMapSeparateValue(n: Int, context: Context): Parser[String] = ???

  /**
    * [148]
    */
  def cNsFlowMapJsonKeyEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [149]
    */
  def cNsFlowMapAdjacentValue(n: Int, context: Context): Parser[String] = ???

  /**
    * [150]
    */
  def nsFlowPair(n: Int, context: Context): Parser[String] = ???

  /**
    * [151]
    */
  def nsFlowPairEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [152]
    */
  def nsFlowPairYamlKeyEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [153]
    */
  def cNsFlowPairJsonKeyEntry(n: Int, context: Context): Parser[String] = ???

  /**
    * [154]
    */
  def nsSImplicitYamlKey(context: Context): Parser[String] = ???

  /**
    * [155]
    */
  def cSImplicitJsonKey(context: Context): Parser[String] = ???

  // Flow Nodes
  /**
    * [156]
    */
  def nsFlowYamlContent(n: Int, context: Context): Parser[String] = ???

  /**
    * [157]
    */
  def cFlowJsonContent(n: Int, context: Context): Parser[String] = ???

  /**
    * [158]
    */
  def nsFlowContent(n: Int, context: Context): Parser[String] = ???

  /**
    * [159]
    */
  def nsFlowYamlNode(n: Int, context: Context): Parser[String] = ???

  /**
    * [160]
    */
  def cFlowJsonNode(n: Int, context: Context): Parser[String] = ???

  /**
    * [161]
    */
  def nsFlowNode(n: Int, context: Context): Parser[String] = ???

}
