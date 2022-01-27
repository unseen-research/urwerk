package urwerk.cli

import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.util.Try
import scala.util.Failure

object Parameter:

  def configOf[C](config: C): ConfigProvider[C] = new ConfigProvider:
      def get = config

  trait ConfigProvider[C]: 
      type CC = C
      def get: CC

  def param[V](using valueSpec: ValueSpec[V], config: ConfigProvider[?]): Parameter[V, config.CC] = 
    new Parameter(Seq(), valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})

  def param[V](using valueSpec: ValueSpec[V], config: ConfigProvider[?])(name: String, names: String*): Parameter[V, config.CC] = 
    new Parameter(name +: names, valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})

  trait ValueSpec[V]:
    type VV = V
    def requireValue: Boolean
    def isValue(value: String): Boolean
    def convert(value: String): V
    def defaultLabel: String

  given ValueSpec[String] with
    val requireValue = true
    def isValue(value: String): Boolean = !value.startsWith("-")
    def convert(value: String): String = value
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    val requireValue = true
    def isValue(value: String): Boolean = 
      !value.startsWith("--")
      //value.nonEmpty && value.toDoubleOption.isDefined
    def convert(value: String): Int = value.toInt
    def defaultLabel: String = "INT"

  given ValueSpec[Boolean] with
    val requireValue = false
    def isValue(value: String): Boolean = value.isEmpty
    def convert(value: String): Boolean = 
      val lowerValue = value.toLowerCase

      if lowerValue.isEmpty then true
      else if lowerValue == "true" then true
      else false

    def defaultLabel: String = "UNIT"

  given [T](using valueSpec: ValueSpec[T]): ValueSpec[Seq[T]] with
    val requireValue = false
    def isValue(value: String): Boolean = value.isEmpty
    def convert(value: String): Seq[T] = ???
    def defaultLabel: String = s"SEQUENCE[${valueSpec.defaultLabel}]"


import Parameter.ValueSpec

class Parameter[V, C](val names: Seq[String],
    val label: String,
    val arity: (Int, Int),
    val default: Option[V],
    val valueRequired: Boolean,
    val isValueOp: String => Boolean,
    val convertOp: String => V,
    val applyOp: (V, C) => C):

  def this(names: Seq[String],
      label: String,
      arity: (Int, Int),
      default: Option[V],
      valueSpec: ValueSpec[V],
      applyOp: (V, C) => C) =
    this(names, label, arity, default, valueSpec.requireValue, valueSpec.isValue, valueSpec.convert, applyOp)

  def default(value: V): Parameter[V, C] = copy(default = Some(value))

  def apply(op: (V, C) => C): Parameter[V, C] =
    copy(applyOp = op)

  def accept(op: String => Boolean): Parameter[V, C] =
    copy(isValueOp = op)

  def arity(minArity: Int, maxArity: Int): Parameter[V, C] =
    copy(arity = (minArity, maxArity))

  def minArity: Int = arity._1

  def maxArity: Int = arity._2

  def label(label: String): Parameter[V, C] =
    copy(label = label)

  def name: String = names.headOption.getOrElse("")

  def name(name: String): Parameter[V, C] =
    copy(names = name +: names.drop(1))

  def names(name: String, names: String*): Parameter[V, C] =
    copy(names = name +: names)

  private def copy(names: Seq[String] = names,
      label: String = label,
      arity: (Int, Int) = arity,
      default: Option[V] = default,
      valueRequired: Boolean = valueRequired,
      isValueOp: String => Boolean = isValueOp,
      convertOp: String => V = convertOp,
      applyOp: (V, C) => C = applyOp) =
    new Parameter(names, label, arity, default, valueRequired, isValueOp, convertOp, applyOp)

  private[cli] def collectValue(config: C, value: String, position: Position): C =
    if !isValueOp(value) then
      throw IllegalValueException(position)
    Try(
        convertOp(value))
      .recoverWith(ex => Failure(IllegalValueException(ex, position)))
      .flatMap(value => applyCollectOp(value, config, position))
      .get

  private[cli] def collectDefault(config: C, position: Position): C =
    default.map(value =>
        applyCollectOp(value, config, position).get)
      .getOrElse(config)

  private def applyCollectOp(value: V, config: C, position: Position): Try[C] =
    Try(
        applyOp(value, config))
      .recoverWith(ex => Failure(IllegalValueException(ex, position)))

class ParameterException(message: String, cause: Option[Throwable], val position: Position)
  extends RuntimeException(message, cause.orNull)

class ArityExceededException(val name: String, val maxArity: Int, position: Position) extends ParameterException("", None, position)

class IllegalValueException(cause: Option[Throwable], position: Position) extends ParameterException("", cause, position):
  def this(position: Position) = this(None, position)
  def this(cause: Throwable, position: Position) = this(Some(cause), position)

class MissingParameterException(val labelOrName: String, val requiredArity: Int, val repetition: Int, position: Position)
  extends ParameterException("", None, position)

class MissingValueException(position: Position)
  extends ParameterException("", None, position)

class UnexpectedParameterException(position: Position) extends ParameterException("", None, position)

case class Position(val index: Int, val subindex: Int)

object ParameterList:
  enum ParamKey:
    case Name(name: String)
    case Pos(pos: Int)

class ParameterList[A](params: Seq[Parameter[?, A]]):
  import ParameterList.*
 

  def collectParams(config: A, args: Seq[String]): (A, Position) =
    collectParams(config, args, Position(0, 0))

  def collectParams(config: A, args: Seq[String], pos: Position): (A, Position) =
    val paramsMap = namedParamMap(params, Map())

    val paramRepetions = {
      val posMap = positionalParamList
        .zipWithIndex.map((p, i)=> (ParamKey.Pos(i), (p, 0)))
        .toMap
      val nameMap = params
        .filter(_.names.nonEmpty)
        .map(p => (ParamKey.Name(p.name), (p, 0)))
      posMap ++ nameMap
    }

    collectParams(args, pos, positionalParamList, config, paramsMap, paramRepetions)

  private def postProcess(config: A, repetitions: Map[ParamKey, (Parameter[?, A], Int)], pos: Position): A =
    val (_config, _repetitions) = applyDefaults(config, repetitions, pos)
    validateArity(_repetitions, pos)
    _config

  private def applyDefaults(config: A, repetitions: Map[ParamKey, (Parameter[?, A], Int)], position: Position): (A, Map[ParamKey, (Parameter[?, A], Int)]) =
    repetitions.foldLeft((config, repetitions)){case ((config, repetitions), (paramKey, (param, repetition))) =>
      if repetition == 0 && param.default.isDefined then
        (param.collectDefault(config, position),
          repetitions.updated(paramKey, (param, 1)))
      else
        (config, repetitions)
    }

  private def validateArity(repetitions: Map[ParamKey, (Parameter[?, A], Int)], pos: Position) =
    repetitions.values.foreach{case (param, repetition) =>
      val requiredArity = param.minArity
      val labelOrName = if param.name.nonEmpty then param.name else param.label
      if repetition < requiredArity then
        throw MissingParameterException(labelOrName, requiredArity = requiredArity, repetition = repetition, pos)
    }

  @tailrec
  private def collectParams(args: Seq[String],
      pos: Position,
      positionalParams: Seq[Parameter[?, A]],
      config: A,
      paramsMap: Map[String, Parameter[?, A]],
      repetitions: Map[ParamKey, (Parameter[?, A], Int)]): (A, Position) =
    val Position(argIndex, flagIndex) = pos
    if argIndex >= args.size then
      val _config = postProcess(config, repetitions, pos)
      (_config, pos)
    else
      val arg = args(argIndex)
      definedName(paramsMap, arg, flagIndex) match
        case ("", _) if positionalParams.isEmpty =>
          val _config = postProcess(config, repetitions, pos)
          (_config, Position(argIndex, flagIndex))
        case ("", _) =>
          val value = arg
          val positionalIndex = positionalParamList.size - positionalParams.size
          val param = positionalParams.head
          val (minArity, maxArity) = param.arity

          val (_, arity) = repetitions(ParamKey.Pos(positionalIndex))

          if !param.isValueOp(value) then
            val _config = postProcess(config, repetitions, pos)
            (_config, pos)
          else if arity + 1 >= maxArity then
            val _config = param.collectValue(config, value, pos)
            val _repetitions = repetitions
              .updatedWith(ParamKey.Pos(positionalIndex)){
                case Some((param, arity)) =>
                  Some((param, arity + 1))
                case None =>
                  ???
              }
            val nextPos = Position(argIndex + 1, 0)
            collectParams(args, nextPos, positionalParams.drop(1), _config, paramsMap, _repetitions)
          else
            val _config = param.collectValue(config, value, pos)
            val _repetitions = repetitions
              .updatedWith(ParamKey.Pos(positionalIndex)){
                case Some((param, arity)) =>
                  Some((param, arity + 1))
                case None => ???}
            val nextPos = Position(argIndex + 1, 0)
            collectParams(args, nextPos, positionalParams, _config, paramsMap, _repetitions)

        case (name, nextFlagIndex) =>
          paramsMap.get(name) match
            case Some(param) =>
              val valueRequired = param.valueRequired
              val value = if valueRequired then
                if name.size == 1
                    && flagIndex + 2 < arg.size then
                  throw new MissingValueException(pos)
                if argIndex +1 >= args.size then
                  throw MissingValueException(pos)
                args(argIndex + 1)
              else ""

              val _config = param.collectValue(config, value, pos)

              val primaryName = param.name
              val _repetitions = repetitions
                .updatedWith(ParamKey.Name(primaryName)){
                  case Some((param, arity)) =>
                    val newArity = arity + 1
                    if newArity > param.maxArity then
                      throw new ArityExceededException(name, param.maxArity, pos)
                    Some((param, newArity))
                  case None => ???
                }

              val nextArgIndex = if nextFlagIndex > 0 then argIndex
              else if value.nonEmpty then argIndex + 2
              else argIndex + 1
              val _pos = Position(nextArgIndex, nextFlagIndex)
              collectParams(args, _pos, positionalParams, _config, paramsMap, _repetitions)
            case None =>
              val _config = postProcess(config, repetitions, pos)
              (_config, Position(argIndex, flagIndex))

  private def definedName(paramsMap: Map[String, Parameter[?, A]], arg: String, flagIndex: Int): (String, Int) =
    val (name, nextFlagIndex) = if arg.startsWith("--") then
      (arg.stripPrefix("--"), 0)
    else if arg.startsWith("-") && arg.size > 1 then
      val nextFlagIndex = if flagIndex + 2 >= arg.size then 0 else flagIndex + 1
      (arg(flagIndex+1).toString, nextFlagIndex)
    else ("", 0)
    if paramsMap.isDefinedAt(name) then (name, nextFlagIndex) else ("", 0)

  private def positionalParamList = params.filter(_.names.isEmpty)

  @tailrec
  private def namedParamMap(params: Seq[Parameter[?, A]], paramsMap: Map[String, Parameter[?, A]]): Map[String, Parameter[?, A]] =
    if params.isEmpty then
      paramsMap
    else
      val param = params.head
      val names = param.names
      val map = names.foldLeft(paramsMap){(map, name) =>
        map.updatedWith(name){
          case Some(_) =>
            throw IllegalArgumentException()
          case None =>
            Some(param)
        }
      }
      namedParamMap(params.tail, map)

