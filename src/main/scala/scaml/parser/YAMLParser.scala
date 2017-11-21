package scaml.parser

import atto._, Atto._
import cats.implicits._

trait YAMLParser[A] {

  def parser: Parser[A]

}

object YAMLParser {

  implicit val stringParser: YAMLParser[String] = new YAMLParser[String] {

    override def parser: Parser[String] = many(noneOf("\n")).map(_.mkString)

  }

  implicit val intParser: YAMLParser[Int] = new YAMLParser[Int] {

    override def parser: Parser[Int] = int

  }

  implicit val booleanParser: YAMLParser[Boolean] = new YAMLParser[Boolean] {

    override def parser: Parser[Boolean] = (string("true") | string("false")).map(_.toBoolean)

  }

  implicit val floatParser: YAMLParser[Float] = new YAMLParser[Float] {

    override def parser: Parser[Float] = floatNumeric | floatInf | floatNaN

    private val afterDot: Parser[String] =
      (string("."), opt(takeWhile1(_.isDigit)), opt(string("e") ~ takeWhile1(_.isDigit)))
        .mapN { case (dot, digitAfterDot, eAndAfter) =>
          dot ++ digitAfterDot
            .map(dad => eAndAfter.map(ed => dad ++ ed._1 ++ ed._2).getOrElse(dad)).getOrElse("")
        }

    private val floatInf: Parser[Float] =
      (opt(char('-')) ~ string(".inf"))
        .map {
          case (Some(_), _) => Float.NegativeInfinity
          case (None,    _) => Float.PositiveInfinity
        }

    private val floatNumeric: Parser[Float] =
      (opt(string("-")), takeWhile1(_.isDigit), opt(afterDot))
        .mapN { case (negOpt, beforeDot, afterDotOpt) =>
          (negOpt.getOrElse("") ++ beforeDot ++ afterDotOpt.getOrElse("")).toFloat
        }

    private val floatNaN: Parser[Float] =
      string(".nan").map(_ => Float.NaN)

  }

  implicit def mapParser[A, B](implicit YAMLParserA: YAMLParser[A], YAMLParserB: YAMLParser[B]): YAMLParser[Map[A, B]] =
    new YAMLParser[Map[A, B]] {

      override def parser: Parser[Map[A, B]] =
        sepBy(keyValue, char('\n')).map(_.toMap)

      private val keyValue =
        (YAMLParserA.parser, many(spaceChar) ~ char(':') ~ many(spaceChar), YAMLParserB.parser)
          .mapN { case (key, _, value) => (key, value) }

    }

  implicit def listParser[A](implicit YAMLParser: YAMLParser[A]): YAMLParser[List[A]] =
    new YAMLParser[List[A]] {

      override def parser: Parser[List[A]] =
        sepBy(string("-") ~ spaceChar ~> YAMLParser.parser, char('\n'))

    }

}
