package urwerk.cli

import urwerk.cli.OptionSpec.OverflowStrategy
import urwerk.source.Source

import java.util.NoSuchElementException
import scala.annotation.tailrec
import OptionSpec.OverflowStrategy
import OptionSpec.OverflowStrategy.*

class OptionSpec[+A](val names: Seq[String],
      val arity: (Int, Int),
      val default: Option[A],
      val overflowStrategy: OverflowStrategy,
      val mapOp: (String) => A):

  import OptionSpec.*

  validateArity(arity)

  def arity(min: Int, max: Int): OptionSpec[A] =
    OptionSpec(names, (min, max), default, overflowStrategy, mapOp)

  def default[A1 >: A](value: A1): OptionSpec[A1] =
    OptionSpec[A1](names, arity, Some(value), overflowStrategy, mapOp)

  def map[A1 >: A](op: (String) => A1): OptionSpec[A1] =
    OptionSpec(names, arity, default, overflowStrategy, op)

  def map(arg: String): A =
    mapOp(arg)

  def names(name: String, names: String*): OptionSpec[A] =
    OptionSpec(name +: names, arity, default, overflowStrategy, mapOp)

  def name: String = names.head

  def overflowStrategy(overflowStrategy: OverflowStrategy): OptionSpec[A] =
    OptionSpec(names, arity, default, overflowStrategy, mapOp)

  private def validateArity(arity: (Int, Int)): Unit =
    val (minArity, maxArity) = arity
    if minArity > maxArity then
      throw IllegalArgumentException(s"minArity is greater than maxArity: minArity=$minArity maxArity=$maxArity")

    if minArity < 0 then
      throw IllegalArgumentException(s"minArity is smaller than zero: minArity=$minArity")

object OptionSpec:

  enum OverflowStrategy:
   case DropFirst,
    DropLast,
    Reject,
    Fail

  final val DefaultOverflowStrategy = OverflowStrategy.DropFirst

  trait Defaults[A]:
    def arity: (Int, Int)
    def map(value: String): A
  

  given Defaults[String] with {
    def arity: (Int, Int) = (0, 1)
    def map(value: String): String = value
  }

  given Defaults[Unit] with {
    def arity: (Int, Int) = (0, 0)
    def map(value: String): Unit = ()
  }

  given Defaults[Boolean] with {
    def arity: (Int, Int) = (0, 1)
    def map(value: String): Boolean = value.toBoolean
  }

def option[A](names: String*)(using defaults: OptionSpec.Defaults[A]): OptionSpec[A] =
  OptionSpec(names, defaults.arity, None, OptionSpec.DefaultOverflowStrategy, arg => defaults.map(arg))

class NoSuchOptionException(val spec: OptionSpec[_]) extends NoSuchElementException(s"Option required: name=${spec.name}")

class NoSuchValueException(val spec: OptionSpec[_]) extends NoSuchElementException(s"Option value required: name=${spec.name}")

class ArityExceededException(val spec: OptionSpec[_]) extends IndexOutOfBoundsException(s"Option arity exceeded: name=${spec.name}, maxArity=${spec.arity._2}")

extension (args: Seq[String])

  def extractOptions(specs: OptionSpec[_]*): (Map[String, Any], Seq[String]) =
    val specsMap = specs.foldLeft(Map[String, OptionSpec[_]]())((specMap, spec) =>
      specMap ++ spec.names.map((_, spec)))

    val (options, argsTail) = _extractOptions(specsMap, args, Map())

    (supplyDefaultValues(specs, options), argsTail)

  private def supplyDefaultValues(specs: Seq[OptionSpec[_]], options: Map[String, Any]): Map[String, Any] =
    specs.foldLeft(options){(options, spec) =>
      val name = spec.name
      val defaultOpt = spec.default
      val opt = options.get(name)
      val (minArity, maxArity) = spec.arity

      if opt.isEmpty && defaultOpt.nonEmpty then
        if maxArity > 1 then
          options.updated(name, Seq(spec.default.get))
        else
          options.updated(name, spec.default.get)
      else if minArity > 0 then
        throw NoSuchOptionException(spec)
      else
        options
    }

@tailrec
private def _extractOptions(specs: Map[String, OptionSpec[_]], argsSeq: Seq[String], options: Map[String, Any]): (Map[String, Any], Seq[String]) =
  if argsSeq.isEmpty then
    (options, argsSeq)
  else
    val (name, tail) = nextName(argsSeq)
    if name.isEmpty then
      (options, argsSeq)
    else
      specs.get(name) match
        case Some(spec) =>
          val name = spec.name
          val (minArity, maxArity) = spec.arity
          if maxArity == 0 then
            _extractOptions(specs, tail, options.updated(name, spec.map("")))
          else
            val (value, nextTail) = nextValue(tail)
            if value.isEmpty then
              throw NoSuchValueException(spec)

            val overflowStrategy = spec.overflowStrategy
            if maxArity == 1 then
              if overflowStrategy == DropFirst || !options.contains(name) then
                _extractOptions(specs, nextTail, options.updated(name, spec.map(value)))
              else if overflowStrategy == DropLast then
                _extractOptions(specs, nextTail, options)
              else if overflowStrategy == Reject then (options, argsSeq)
              else throw ArityExceededException(spec)
            else
              val newValue = spec.map(value)
              val currentSeq = options.getOrElse(name, Seq()).asInstanceOf[Seq[_]]
              if currentSeq.size + 1 > maxArity then
                overflowStrategy match
                  case DropFirst =>
                    _extractOptions(specs, nextTail, options.updated(name, currentSeq.drop(1) :+ newValue))
                  case DropLast =>
                    _extractOptions(specs, nextTail, options.updated(name, currentSeq))
                  case Reject =>
                    (options, argsSeq)
                  case Fail => throw ArityExceededException(spec)
              else
                _extractOptions(specs, nextTail, options.updated(name, currentSeq :+ newValue))
        case None =>
          (options, argsSeq)

def nextName(args: Seq[String]): (String, Seq[String]) =
  if args.isEmpty then ("", args)
  else
    val (prefix, suffix) = args.head.span(_ == '-')
    if prefix.size == 0 then
      ("", args)
    else if prefix.size == 1 && suffix.size > 0 then
      val names = suffix.map(_.toString)
      val name = names.head
      val leftFlags = names.drop(1).mkString

      if leftFlags.nonEmpty then
        (name, ("-" + leftFlags) +: args.drop(1))
      else
        (name, args.drop(1))
    else if prefix.size > 1 then
      (suffix, args.drop(1))
    else
      ("", args)

def nextValue(args: Seq[String]): (String, Seq[String]) =
  if args.isEmpty || args.head.startsWith("-") then ("", args)
  else (args.head, args.drop(1))

def resolveCommand(args: Seq[String]): Source[String] = ???