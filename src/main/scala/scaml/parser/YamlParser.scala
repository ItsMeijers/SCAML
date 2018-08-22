package scaml.parser

import atto._
import Atto._
import cats.Applicative
import cats.data.{NonEmptyList, StateT}
import cats.syntax.show._
import YamlParseError.YamlParseErrorShowInstance

case class YamlState(currentIndentation: Int, context: Context)

case class YamlParser[A](run: StateT[Parser, YamlState, A]) {

  def map[B](f: A => B): YamlParser[B] =
    YamlParser(run.map(f))

  def flatMap[B](f: A => YamlParser[B]): YamlParser[B] =
    YamlParser(run.flatMap(f(_).run))

  def getState: YamlParser[YamlState] =
    YamlParser.mapState(identity)

  def setState(newState: YamlState): YamlParser[Unit] =
    YamlParser.mapStateNew(ys => (ys, ()))

  def opt: YamlParser[Option[A]] =
    YamlParser.optional(this)

  def |(that: YamlParser[A]): YamlParser[A] =
    YamlParser.fromCurrentState { (ys: YamlState) =>
      this.run.run(ys) | that.run.run(ys)
    }

  def many1: YamlParser[NonEmptyList[A]] =
    YamlParser(StateT[Parser, YamlState, NonEmptyList[A]] { (ys: YamlState) =>
      for {
        nel <- this.run.run(ys).many1
      } yield (nel.last._1, nel.map(_._2))
    })

  def many: YamlParser[List[A]] =
    YamlParser(StateT[Parser, YamlState, List[A]] { (ys: YamlState) =>
      for {
        xs <- this.run.run(ys).many // TODO abstract over many && many1
      } yield (xs.last._1, xs.map(_._2))
    })

}

object YamlParser {

  implicit def applicativeInstance(implicit app: Applicative[StateT[Parser, YamlState, ?]]): Applicative[YamlParser] =
    new Applicative[YamlParser] {
      override def pure[A](x: A): YamlParser[A] = YamlParser[A]((ys: YamlState) => (ys, ok(x)))

      override def ap[A, B](ff: YamlParser[A => B])(fa: YamlParser[A]): YamlParser[B] = {
        val ffState = ff.run
        val faState = fa.run
        YamlParser(app.ap(ffState)(faState))
      }
    }

  def apply[A](f: YamlState => (YamlState, Parser[A])): YamlParser[A] =
    YamlParser(StateT[Parser, YamlState, A](ys => {
      val (nys, p) = f(ys)
      p.map(a => (nys, a))
    }))

  def fromState[A](f: YamlState => Parser[Option[(YamlState, A)]]): YamlParser[Option[A]] =
    YamlParser(StateT[Parser, YamlState, Option[A]](ys =>
      f(ys).map {
        case None => (ys, None)
        case Some((ns, r)) => (ns, Some(r))
      }))

  def pure[A](a: A): YamlParser[A] =
    pureP(ok(a))

  def pureP[A](parser: Parser[A]): YamlParser[A] =
    fromCurrentState(ys => parser.map((ys, _)))

  def fromCurrentIndentation[A](fromCurrentIdent: (Int) => (Int, Parser[A])): YamlParser[A] =
    fromCurrentState(ys => {
      val (newIdent, parser) = fromCurrentIdent(ys.currentIndentation)
      parser.map((ys.copy(currentIndentation = newIdent), _))
    })

  def setContext(context: Context): YamlParser[Unit] =
    mapStateNew(ys => (ys.copy(context = context), ()))

  def fromCurrentState[A](f: YamlState => Parser[(YamlState, A)]): YamlParser[A] =
    YamlParser(StateT[Parser, YamlState, A](f))

  def mapState[A](f: YamlState => A): YamlParser[A] =
    YamlParser(StateT[Parser, YamlState, A](s => ok(s, f(s))))

  def mapStateNew[A](f: YamlState => (YamlState, A)): YamlParser[A] =
    fromCurrentState(ys => ok(f(ys)))

  def getIndentation: YamlParser[Int] =
    mapState(_.currentIndentation)

  def getContext: YamlParser[Context] =
    mapState(_.context)

  def fail[A](yamlParseError: YamlParseError): YamlParser[A] =
    YamlParser(StateT[Parser, YamlState, A](_ => err(yamlParseError.show)))

  def optional[A](yamlParser: YamlParser[A]): YamlParser[Option[A]] = YamlParser
    .fromState { yamlState: YamlState =>
      opt(yamlParser
        .run
        .run(yamlState))
    }

  def startOfLine: YamlParser[Unit] = for {
    indentation <- getIndentation
  } yield if(indentation == 0) YamlParser.pure[Unit](())
          else YamlParser.fail[Unit](YamlIndentationError(indentation))


}
