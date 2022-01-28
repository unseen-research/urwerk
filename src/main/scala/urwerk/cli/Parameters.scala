package urwerk.cli

import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.util.Try
import scala.util.Failure
import java.io.ObjectInputFilter.Config

trait ConfigEvidence[C]:
  type CC = C

def config[C] = new ConfigEvidence{}

object Parameter:

  def param[V](using valueSpec: ValueSpec[V], config: ConfigEvidence[?]): Parameter[V, config.CC] = 
    new Parameter(Seq(), valueSpec.defaultLabel, None, valueSpec, {(_, config) => config})

  def param[V](using valueSpec: ValueSpec[V], config: ConfigEvidence[?])(name: String, names: String*): Parameter[V, config.CC] = 
    new Parameter(name +: names, valueSpec.defaultLabel, None, valueSpec, {(_, config) => config})

  trait ValueSpec[V]:
    type VV = V
    def requireValue: Boolean
    def isValue(value: String): Boolean
    def defaultValue: Option[V]    
    def convert(value: String): V
    def convertSeq(values: Seq[String]): V
    def defaultLabel: String

  given ValueSpec[String] with
    val requireValue = true
    def isValue(value: String): Boolean = !value.startsWith("-")
    def defaultValue: Option[String] = None
    def convert(value: String): String = value
    def convertSeq(values: Seq[String]): String = convert(values.last)
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    val requireValue = true
    def isValue(value: String): Boolean = 
      !value.startsWith("--")
      //value.nonEmpty && value.toDoubleOption.isDefined
    def defaultValue: Option[Int] = None
    def convert(value: String): Int = value.toInt
    def convertSeq(values: Seq[String]): Int = convert(values.last)
    def defaultLabel: String = "INT"

  given ValueSpec[Boolean] with
    val requireValue = false
    def isValue(value: String): Boolean = value.isEmpty
    def defaultValue: Option[Boolean] = Some(true)
    def convert(value: String): Boolean = 
      val lowerValue = value.toLowerCase
      if lowerValue.isEmpty then true
      else if lowerValue == "true" then true
      else false
    def convertSeq(values: Seq[String]): Boolean = convert(values.last)
    def defaultLabel: String = "BOOLEAN"

  given [T](using valueSpec: ValueSpec[T]): ValueSpec[Seq[T]] with
    val requireValue = false
    def isValue(value: String): Boolean = value.isEmpty
    def defaultValue: Option[Seq[T]] = None
    def convert(value: String): Seq[T] = Seq(valueSpec.convert(value))
    def convertSeq(values: Seq[String]): Seq[T] = 
      values.map(valueSpec.convert(_))
    def defaultLabel: String = s"SEQUENCE[${valueSpec.defaultLabel}]"

import Parameter.ValueSpec

case class Parameter[V, C](val names: Seq[String],
    val label: String,
    val default: Option[V],
    val valueSpec: ValueSpec[V],
    val applyOp: (V, C) => C):

  def default(value: V): Parameter[V, C] = copy(default = Some(value))

  def apply(op: (V, C) => C): Parameter[V, C] =
    copy(applyOp = op)

  def label(label: String): Parameter[V, C] =
    copy(label = label)

  def name: String = names.headOption.getOrElse("")

  def name(name: String): Parameter[V, C] =
    copy(names = name +: names.drop(1))

  def names(name: String, names: String*): Parameter[V, C] =
    copy(names = name +: names)

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

case class Position(val argIndex: Int, val flagIndex: Int)

object ParameterList:
  enum ParamKey:
    case Name(name: String)
    case Pos(pos: Int)

  private enum Arg:
    case NameArg(name: String, nextPos: Position)
    case ValueArg(value: String, nextPos: Position)
    case UndefinedArg(nextPos: Position)

  extension [C](paramList: ParameterList[C])
    def collect(config: C, args: Seq[String]): (C, Position) = 
      collectParams(config, paramList, args)

  private def collectParams[C](config: C, paramList: ParameterList[C], args: Seq[String]): (C, Position) =
    val params = paramList.params
    val positionalParams = positionalParameters(params)
    val namedParams = namedParameters(params)
    val (collectedParams, pos) = paramList.collectParams(args, positionalParams, namedParams, Map(), Position(0, 0), 0)
    val updatedConfig = collectedParams.values.foldLeft(config){case (config, (param, values)) =>
      val value = param.valueSpec.convertSeq(values)
      param.applyOp(value, config)
    }
    (updatedConfig, pos)

  private def positionalParameters[C](params: Seq[Parameter[?, C]]) = params.filter(_.names.isEmpty)

  private def namedParameters[C](params: Seq[Parameter[?, C]]): Map[String, Parameter[?, C]] =
    namedParameters(params, Map())

  @tailrec
  private def namedParameters[C](params: Seq[Parameter[?, C]], paramsMap: Map[String, Parameter[?, C]]): Map[String, Parameter[?, C]] =
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
      namedParameters(params.tail, map)

class ParameterList[A](val params: Seq[Parameter[?, A]]):
  import ParameterList.*
  import ParameterList.Arg.*

  def parameter(param: ConfigEvidence[A] ?=> Parameter[?, A]): ParameterList[A] = 
    given ConfigEvidence[A] = new ConfigEvidence[A]{}
   
    val resolvedParam = param
    ParameterList(params :+ resolvedParam)

  @tailrec
  private def collectParams(
      args: Seq[String],
      positionalParams: Seq[Parameter[?, A]],
      namedParms: Map[String, Parameter[?, A]],
      actualParamms: Map[ParamKey, (Parameter[?, A], Seq[String])],
      pos: Position,
      positionalIndex: Int): (Map[ParamKey, (Parameter[?, A], Seq[String])], Position) =
    
    //val Position(argIndex, flagIndex) = argPos
    if pos.argIndex >= args.size then
      //val _config = postProcess(config, repetitions, pos)
      (actualParamms, pos)
    else
      //val arg = args(argIndex)
      nextArg(namedParms, args, pos) match
        case ValueArg(_, nextPos) if positionalParams.size <= positionalIndex =>
          //val _config = postProcess(config, repetitions, pos)
          (actualParamms, nextPos)
        case ValueArg(value, nextPos) =>
          val param = positionalParams(positionalIndex)
          val valSpec = param.valueSpec
          if !valSpec.isValue(value) then
            (actualParamms, pos)
          else
            val updatedParams = actualParamms.updatedWith(ParamKey.Pos(positionalIndex)){
              case Some((param, values)) =>
                Some((param, values :+ value))
              case None => 
                Some((param, Seq(value)))
            }
            collectParams(args, positionalParams, namedParms, updatedParams , nextPos, positionalIndex+1)
        case NameArg(name, nextPos) =>
          val param = namedParms(name)
          val prevPos = nextPos
          nextArg(namedParms, args, nextPos) match
            case NameArg(name, _) =>
              val value = ""
              if !param.valueSpec.isValue(value) then
                throw IllegalValueException(nextPos)

              val updatedParams = actualParamms.updatedWith(ParamKey.Name(name)){
                case Some((param, values)) =>
                  Some((param, values))
                case None => 
                  Some((param, Seq(value)))
              }
              collectParams(args, positionalParams, namedParms, updatedParams , nextPos, positionalIndex)
            case ValueArg(value, nextPos) =>
              if !param.valueSpec.isValue(value) then
                throw IllegalValueException(prevPos)

              val updatedParams = actualParamms.updatedWith(ParamKey.Name(name)){
                case Some((param, values)) =>
                  Some((param, values))
                case None => 
                  Some((param, Seq(value)))
              }
              collectParams(args, positionalParams, namedParms, updatedParams , nextPos, positionalIndex)
            case UndefinedArg(nextPos) =>  ???     
        case UndefinedArg(nextPos) =>
          (actualParamms, nextPos)    
  
  private def nextArg(params: Map[String, Parameter[?, A]], args: Seq[String], pos: Position): Arg =
    val Position(argIndex, flagIndex) = pos
    val arg = args(argIndex)

    if arg.startsWith("--") then
      params.get(arg.stripPrefix("--")) match
        case Some(param) => 
          NameArg(param.name, Position(argIndex+1, 0))
        case None => UndefinedArg(pos)

    else if arg.startsWith("-") && arg.size > 1 && arg.size <= flagIndex+2 then
      nextArg(params, args, Position(argIndex+1, 0))

    else if arg.startsWith("-") && arg.size > 1 then
      val name = arg(flagIndex+1).toString
      val nextPos = if flagIndex+2 >= arg.size then Position(argIndex+1, 0) else Position(argIndex, flagIndex+1)
      params.get(name) match
        case Some(param) => 
          NameArg(param.name, nextPos)
        case None => UndefinedArg(pos)

    else 
      ValueArg(arg, Position(argIndex+1, 0))

