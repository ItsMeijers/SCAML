package scaml.parser

import atto._
import Atto._
import cats.syntax.applicative._
import YamlParser.applicativeInstance
import CharacterParsers._
import cats.data.{EitherT, State, StateT}

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
  *
  * In YAML block styles, structure is determined by indentation. In general, indentation is defined as a zero or more
  * space characters at the start of a line.
  **
  *To maintain portability, tab characters must not be used in indentation, since different systems treat tabs differently.
  *Note that most modern editors may be configured so that pressing the tab key results in the insertion of an appropriate
  *number of spaces.
  */
object BasicStructureParsers {

  // Indentation Spaces: In YAML block styles, structure is determined by indentation. In general, indentation is
  // defined as a zero or more space characters at the start of a line. To maintain portability, tab characters must
  // not be used in indentation, since different systems treat tabs differently.

  /**
    * [63, 64, 65] The amount of indentation is a presentation detail and must not be used to convey content information.
    * A block style construct is terminated when encountering a line which is less indented than the construct. The
    * productions use the notation “s-indent(<n)” and “s-indent(≤n)” to express this.
    */
  def sIndent: YamlParser[Unit] =
    for {
      n <- YamlParser.getIndentation
    } yield YamlParser.fromCurrentIndentation[Unit]((m: Int) => {
        val p = if (m <= n) manyN(m, sSpace) else manyN(n, sSpace)
        (n+m, p.map(_ => ()))
      })

  // Separation Spaces

  /**
    * [66] Outside indentation and scalar content, YAML uses white space characters for separation between tokens within
    * a line. Note that such white space may safely include tab characters. Separation spaces are a presentation detail
    * and must not be used to convey content information.
    */
  val sSeparateInLine: YamlParser[Unit] =
    YamlParser.pureP[Unit](many1(sWhite).void) | YamlParser.startOfLine

  // Line Prefixes Inside scalar content, each line begins with a non-content line prefix. This prefix always includes
  // the indentation. For flow scalar styles it additionally includes all leading white space, which may contain tab
  // characters. Line prefixes are a presentation detail and must not be used to convey content information.

  /**
    * [67] Calls sBlockLinePrefix or sFlowLinePrefix based on the Context.
    */
  def sLinePrefix: YamlParser[Unit] =
    YamlParser.getContext.flatMap {
      case BlockOut => sBlockLinePrefix
      case BlockIn  => sBlockLinePrefix
      case FlowOut  => sFlowLinePrefix
      case FlowIn   => sFlowLinePrefix
      case c        => YamlParser.fail(WrongContextStateError(c))
    }

  /**
    * [68] Line prefix when in a Block Context.
    */
  val sBlockLinePrefix: YamlParser[Unit] =
    sIndent

  /**
    * [69] Line prefix when in a Flow Context.
    */
  def sFlowLinePrefix: YamlParser[Unit] =
    for {
      _ <- sIndent
      _ <- sSeparateInLine.opt
    } yield ()

  // Empty Lines: An empty line line consists of the non-content prefix followed by a line break.

  /**
    * [70] The semantics of empty lines depend on the scalar style they appear in. This is handled on a case-by-case
    * basis by the relevant productions.
    */
  def lEmpty: YamlParser[String] =
    for {
      _       <- sLinePrefix | sIndent
      _       <- YamlParser.pureP(bAsLineFeed)
    } yield "\n"

  // Line Folding: Line folding allows long lines to be broken for readability, while retaining the semantics of the
  // original long line.

  /**
    * [71] If a line break is followed by an empty line, it is trimmed; the first line break is discarded and the rest
    * are retained as content.
    */
  def blTrimmed: YamlParser[String] =
    for {
      _ <- YamlParser.pureP(bNonContent)
      empties <- lEmpty.many1
    } yield empties.toList.mkString

  /**
    * [72] Otherwise (the following line is not empty), the line break is converted to a single space (#x20).
    */
  val bAsSpace: Parser[String] = bBreak

  /**
    * [73] A folded non-empty line may end with either of the above line breaks.
    */
  def blFolded: YamlParser[String] = blTrimmed | YamlParser.pureP(bAsSpace)

  /**
    * [74] The combined effect of the flow line folding rules is that each “paragraph” is interpreted as a line, empty
    * lines are interpreted as line feeds, and text can be freely more-indented without affecting the content
    * information.
    */
  def sFlowFolded: YamlParser[String] =
    for {
      _      <- sSeparateInLine.opt
      _      <- YamlParser.setContext(FlowIn)
      folded <- blFolded
      _      <- sFlowLinePrefix
    } yield folded

  // Comments: An explicit comment is marked by a “#” indicator. Comments are a presentation detail and must not be used
  // to convey content information.

  /**
    * [75] An explicit comment is marked by a "#" indicator followed by zero or many characters
    */
  val cNbCommentText: Parser[Unit] =
    for {
      _ <- string("#")
      _ <- nbChar.many
    } yield ()

  /**
    * [76] To ensure JSON compatibility, YAML processors must allow for the omission of the final comment line break of
    * the input stream.
    */
  val bComment: YamlParser[Unit] = YamlParser.pureP(bNonContent.void | endOfInput)

  /**
    * [77] However, as this confuses many tools, YAML processors should terminate the stream with an explicit line break
    * on output.
    */
  val sBComment: YamlParser[Unit] =
    for {
      _ <- (for {
        _ <- sSeparateInLine
        _ <- YamlParser.pureP(cNbCommentText)
      } yield ()).opt
      _ <- bComment
    } yield ()

  /**
    * [78] Outside scalar content, comments may appear on a line of their own, independent of the indentation level.
    * Note that outside scalar content, a line containing only white space characters is taken to be a comment line.
    */
  val lComment: YamlParser[Unit] =
    for {
      _ <- sSeparateInLine
      _ <- YamlParser.pureP(cNbCommentText).opt
      _ <- bComment
    } yield ()

  /**
    * [79] In most cases, when a line may end with a comment, YAML allows it to be followed by additional comment lines.
    * The only exception is a comment ending a block scalar header.
    */
  val slComments: YamlParser[Unit] =
    for {
      _ <- sBComment | YamlParser.startOfLine
      _ <- lComment.many
    } yield ()

  // Separation Lines: Implicit keys are restricted to a single line. In all other cases, YAML allows tokens to be
  // separated by multi-line (possibly empty) comments.

  /**
    * [80] Pending on the context a different rule is applied for separation lines.
    */
  val sSeparate: YamlParser[Unit] =
    YamlParser.getContext.flatMap {
      case BlockOut => sSeparateLines
      case BlockIn  => sSeparateLines
      case FlowOut  => sSeparateLines
      case FlowIn   => sSeparateLines
      case BlockKey => sSeparateInLine
      case FlowKey  => sSeparateInLine
    }

  /**
    * [81] Note that structures following multi-line comment separation must be properly indented, even though there is
    * no such restriction on the separation comment lines themselves.
    */
  def sSeparateLines: YamlParser[Unit] = {
    val slCommentsWithFlowLinePrefix =
      for {
        _ <- slComments
        _ <- sFlowLinePrefix
      } yield ()

    slCommentsWithFlowLinePrefix | sSeparateInLine
  }

  // Directives: Directives are instructions to the YAML processor. This specification defines two directives, “YAML”
  // and “TAG”, and reserves all other directives for future use. There is no way to define private directives. This is
  // intentional. Directives are a presentation detail and must not be used to convey content information.

  /**
    * [87] Denotes the version of YAML (currently 1.1 and 1.2 are compatible raise warning on higher minor version and
    * reject on higher major version)
    */
  val nsYamlVersion: YamlParser[Double] =
    YamlParser.pureP(for {
      bd <- nsDecDigit.many1
      d  <- string(".")
      ad <- nsDecDigit.many1
    } yield (bd.toList.mkString ++ d ++ ad.toList.mkString).toDouble)


  /**
    * [86] The “YAML” directive specifies the version of YAML the document conforms to. This specification defines
    * version “1.2”, including recommendations for YAML 1.1 processing.
    */
  val nsYamlDirective: YamlParser[Directive] =
    for {
      _       <- YamlParser.pureP(string("YAML"))
      _       <- sSeparateInLine
      version <- nsYamlVersion
    } yield YamlDirective(version)


  /**
    * [94] If the prefix begins with a “!” character, shorthands using the handle are expanded to a local tag. Note that
    * such a tag is intentionally not a valid URI, and its semantics are specific to the application. In particular, two
    * documents in the same stream may assign different semantics to the same local tag.
    */
  val cNsLocalTagPrefix: YamlParser[String] =
    YamlParser.pureP(for {
      _      <- char('!')
      prefix <- nsUriChar.many
    } yield prefix.mkString)

  /**
    * [95] If the prefix begins with a character other than “!”, it must to be a valid URI prefix, and should contain at
    * least the scheme and the authority. Shorthands using the associated handle are expanded to globally unique URI
    * tags, and their semantics is consistent across applications. In particular, every documents in every stream must
    * assign the same semantics to the same global tag.
    */
  val nsGlobalTagPrefix: YamlParser[String] =
    YamlParser.pureP(for {
      _      <- nsTagChar
      prefix <- nsUriChar.many
    } yield prefix.mkString)

  /**
    * [93] There are two tag prefix variants
    */
  val nsTagPrefix: YamlParser[String] = cNsLocalTagPrefix | nsGlobalTagPrefix

  /**
    * [90] The primary tag handle is a single “!” character. This allows using the most compact possible notation for a
    * single “primary” name space. By default, the prefix associated with this handle is “!”. Thus, by default,
    * shorthands using this handle are interpreted as local tags. It is possible to override the default behavior by
    * providing an explicit “TAG” directive, associating a different prefix for this handle. This provides smooth
    * migration from using local tags to using global tags, by the simple addition of a single “TAG” directive.
    */
  val cPrimaryTagHandle: YamlParser[TagHandle] =
    YamlParser.pureP(string("!").map(_ => PrimaryTagHandle))

  /**
    * [91] The secondary tag handle is written as “!!”. This allows using a compact notation for a single “secondary"
    * name space. By default, the prefix associated with this handle is “tag:yaml.org,2002:”. This prefix is used by the
    * YAML tag repository. It is possible to override this default behavior by providing an explicit “TAG” directive
    * associating a different prefix for this handle.
    */
  val cSecondaryTagHandle: YamlParser[TagHandle] =
    YamlParser.pureP(string("!!").map(_ => SecondaryTagHandle))

  /**
    * [92] A named tag handle surrounds a non-empty name with “!” characters. A handle name must not be used in a tag
    * shorthand unless an explicit “TAG” directive has associated some prefix with it. The name of the handle is a
    * presentation detail and must not be used to convey content information. In particular, the YAML processor need not
    * preserve the handle name once parsing is completed.
    */
  val cNamedTagHandle: YamlParser[TagHandle] =
    YamlParser.pureP(for {
      _    <- char('!')
      name <- nsWordChar.many1
      _    <- char('!')
    } yield NamedTagHandle(name.toList.mkString))

  /**
    * [89] The tag handle exactly matches the prefix of the affected tag shorthand. There are three tag handle variants:
    */
  val cTagHandle: YamlParser[TagHandle] =
    cNamedTagHandle | cSecondaryTagHandle | cPrimaryTagHandle

  /**
    * [88] The “TAG” directive establishes a tag shorthand notation for specifying node tags. Each “TAG” directive
    * associates a handle with a prefix. This allows for compact and readable tag notation.
    */
  val nsTagDirective: YamlParser[Directive] =
    for {
      _      <- YamlParser.pureP(string("TAG"))
      _      <- sSeparateInLine
      handle <- cTagHandle
      _      <- sSeparateInLine
      prefix <- nsTagPrefix
    } yield TagDirective(handle, prefix)

  /**
    * [84] The name of the directive
    */
  val nsDirectiveName: YamlParser[String] =
    YamlParser.pureP(nsChar).many1.map(_.toList.mkString)

  /**
    * [85] The parameter of the directive
    */
  val nsDirectiveParameter: YamlParser[String] =
    YamlParser.pureP(nsChar).many1.map(_.toList.mkString)


  /**
    * [83] A reserved directive parser
    */
  val nsReservedDirective: YamlParser[Directive] =
    for {
      name       <- nsDirectiveName
      parameters <- sSeparateInLine.flatMap(_ => nsDirectiveParameter).many
    } yield ReservedDirective(name, parameters)

  /**
    * [82] Each directive is specified on a separate non-indented line starting with the “%” indicator, followed by the
    * directive name and a list of parameters. A YAML processor should ignore unknown directives with an appropriate
    * warning.
    */
  val lDirective: YamlParser[Directive] =
    for {
      _ <- YamlParser.pureP(cDirectiveChar)
      d <- nsYamlDirective | nsTagDirective | nsReservedDirective
      _ <- slComments
    } yield d


  // Node Properties: Each node may have two optional properties, anchor and tag, in addition to its content.

  /**
    * [98] A tag may be written verbatim by surrounding it with the “<” and “>” characters. In this case, the YAML
    * processor must deliver the verbatim tag as-is to the application. In particular, verbatim tags are not subject to
    * tag resolution. A verbatim tag must either begin with a “!” (a local tag) or be a valid URI (a global tag).
    */
  val cVerbatimTag: Parser[String] =
    for {
      _        <- string("!<")
      verbatim <- nsUriChar.many1
      _        <- char('>')
    } yield ??? ///VerbatimTag(verbatim)

  /**
    * [99] A tag shorthand consists of a valid tag handle followed by a non-empty suffix. The tag handle must be
    * associated with a prefix, either by default or by using a “TAG” directive. The resulting parsed tag is the
    * concatenation of the prefix and the suffix, and must either begin with “!” (a local tag) or be a valid URI
    * (a global tag). Choice of tag hanle is a presentation detail.
    */
  val cNsShorthandTag: YamlParser[String] =
    for {
      handle <- cTagHandle
      suffix <- YamlParser.pureP(nsTagChar).many1
    } yield ??? //ShorthandTag(handle, suffix)

  /**
    * [100] If a node has no tag property, it is assigned a non-specific tag that needs to be resolved to a specific one.
    * This non-specific tag is “!” for non-plain scalars and “?” for all other nodes. This is the only case where the
    * node style has any effect on the content information.
    */
  val cNonSpecificTag: Parser[String] = string("!")

  /**
    * [97] The tag property identifies the type of the native data structure presented by the node. A tag is denoted by
    * the “!” indicator.
    */
  val cNsTagProperty: Parser[String] = ???

  /**
    * [96] Node properties may be specified in any order before the node’s content. Either or both may be omitted.
    */
  def cNsProperties(n: Int, context: Context): Parser[String] = ???

  /**
    * [101] An anchor is denoted by the “&” indicator. It marks a node for future reference. An alias node can then be
    * used to indicate additional inclusions of the anchored node. An anchored node need not be referenced by any alias
    * nodes; in particular, it is valid for all nodes to be anchored.
    */
  val cNsAnchorProperty: Parser[String] = ???

  /**
    * [102] Anchor names must not contain the “[”, “]”, “{”, “}” and “,” characters. These characters would cause
    * ambiguity with flow collection structures.
    */
  val nsAnchorChar: Parser[String] = ???

  /**
    * [103] Any possible name of an anchor.
    */
  val nsAnchorName: Parser[String] = ???

}
