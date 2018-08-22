package scaml.parser

import atto._
import Atto._
import cats.implicits._

/**
  * YAML’s block styles employ indentation rather than indicators to denote structure. This results in a more human
  * readable (though less compact) notation.
  */
object BlockStyleParsers {

  // Block Scalar Styles

  /**
    * [162]
    */
  def cBBlockHeader(m: Int, t: BlockChomping): Parser[String] = ???

  /**
    * [163]
    */
  def cIndentationIndicator(m: Int): Parser[String] = ???

  /**
    * [164]
    */
  def cChompingIndicator(t: BlockChomping): Parser[String] = ???

  /**
    * [165]
    */
  def bChompedLast(t: BlockChomping): Parser[String] = ???

  /**
    * [166]
    */
  def lChompedEmpty(n: Int, t: BlockChomping): Parser[String] = ???

  /**
    * [167]
    */
  def lStripEmpty(n: Int): Parser[String] = ???

  /**
    * [168]
    */
  def lKeepEmpty(n: Int): Parser[String] = ???

  /**
    * [169]
    */
  def lTrailComments(n: Int): Parser[String] = ???

  /**
    * [170]
    */
  def cLLiteral(n: Int): Parser[String] = ???

  /**
    * [171]
    */
  def lNbLiteralText(n: Int): Parser[String] = ???

  /**
    * [172]
    */
  def bNbLiteralNext(n: Int): Parser[String] = ???

  /**
    * [173]
    */
  def lLiteralContent(n: Int, t: BlockChomping): Parser[String] = ???

  /**
    * [174]
    */
  def cLFolded(n: Int): Parser[String] = ???

  /**
    * [175]
    */
  def sNbFoldedText(n: Int): Parser[String] = ???

  /**
    * [176]
    */
  def lNbFoldedLines(n: Int): Parser[String] = ???

  /**
    * [177]
    */
  def sNbSpacedText(n: Int): Parser[String] = ???

  /**
    * [178]
    */
  def bLSpaced(n: Int): Parser[String] = ???

  /**
    * [179]
    */
  def lNbSpacedLines(n: Int): Parser[String] = ???

  /**
    * [180]
    */
  def lNbSameLines(n: Int): Parser[String] = ???

  /**
    * [181]
    */
  def lNbDiffLines(n: Int): Parser[String] = ???

  /**
    * [182]
    */
  def lFoldedContent(n: Int, t: BlockChomping): Parser[String] = ???

  // Block Collection Styles

  /**
    * [183]
    */
  def lBlockSequence(n: Int): Parser[String] = ???

  /**
    * [184]
    */
  def cLBlockSeqEntry(n: Int): Parser[String] = ???

  /**
    * [185]
    */
  def sLBlockIndented(n: Int, context: Context): Parser[String] = ???

  /**
    * [186]
    */
  def nsLCompactSequence(n: Int): Parser[String] = ???

  /**
    * [187]
    */
  def lBlockMapping(n: Int): Parser[String] = ???

  /**
    * [188]
    */
  def nsLBlockMapEntry(n: Int): Parser[String] = ???

  /**
    * [189]
    */
  def cLBlockMapExplicitEntry(n: Int): Parser[String] = ???

  /**
    * [190]
    */
  def cLBlockMapExplicitKey(n: Int): Parser[String] = ???

  /**
    * [191]
    */
  def lBlockMapExplicitValue(n: Int): Parser[String] = ???

  /**
    * [192]
    */
  def nsLBlockMapLImplicitEntry(n: Int): Parser[String] = ???

  /**
    * [193]
    */
  val nsSBlockMapImplicitKey: Parser[String] = ???

  /**
    * [194]
    */
  def cLBlockMapImplicitValue(n: Int): Parser[String] = ???

  /**
    * [195]
    */
  def nsLCompactMapping(n: Int): Parser[String] = ???

  /**
    * [196]
    */
  def sLBlockNode(n: Int, context: Context): Parser[String] = ???

  /**
    * [197]
    */
  def sLFlowInBlock(n: Int): Parser[String] = ???

  /**
    * [198]
    */
  def sLBlockInBlock(n: Int, context: Context): Parser[String] = ???

  /**
    * [199]
    */
  def sLBlockScalar(n: Int, context: Context): Parser[String] = ???

  /**
    * [200]
    */
  def sLBlockCollection(n: Int, context: Context): Parser[String] = ???

  /**
    * [201]
    */
  def seqSpaces(n: Int, context: Context): Parser[String] = ???

}
