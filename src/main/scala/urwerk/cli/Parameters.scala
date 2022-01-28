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
    def isValue(value: String): Boolean
    def defaultValue: Option[String]    
    def convert(value: String): V
    def convertSeq(values: Seq[String]): V
    def defaultLabel: String

  given ValueSpec[String] with
    def isValue(value: String): Boolean = true
    def defaultValue: Option[String] = None
    def convert(value: String): String = value
    def convertSeq(values: Seq[String]): String = convert(values.last)
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    def isValue(value: String): Boolean = 
      value.nonEmpty && value.toDoubleOption.isDefined
    def defaultValue: Option[String] = None
    def convert(value: String): Int = value.toInt
    def convertSeq(values: Seq[String]): Int = convert(values.last)
    def defaultLabel: String = "INT"

  given ValueSpec[Boolean] with
    def isValue(value: String): Boolean = 
      val lowerVal = value.toLowerCase
      lowerVal == "true" || lowerVal == "false" 

    def defaultValue: Option[String] = Some("true")
    def convert(value: String): Boolean = 
      val lowerValue = value.toLowerCase
      if lowerValue.isEmpty then true
      else if lowerValue == "true" then true
      else if lowerValue == "false" then false
      else throw IllegalArgumentException()
    def convertSeq(values: Seq[String]): Boolean = convert(values.last)
    def defaultLabel: String = "BOOLEAN"

  given [T](using valueSpec: ValueSpec[T]): ValueSpec[Seq[T]] with
    def isValue(value: String): Boolean = value.isEmpty
    def defaultValue: Option[String] = None
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
    extends RuntimeException(message, cause.orNull):
  def this(message: String, cause: Throwable, position: Position) = this(message, Some(cause), position)
  def this(cause: Throwable, position: Position) = this("", Some(cause), position)

class IllegalValueException() extends RuntimeException()

// class MissingParameterException(val labelOrName: String, val requiredArity: Int, val repetition: Int, position: Position)
//   extends ParameterException("", None, position)

class MissingValueException() extends RuntimeException()

//class UnexpectedParameterException(position: Position) extends ParameterException("", None, position)

case class Position(val argIndex: Int, val flagIndex: Int)

object ParameterList:
  enum ParamKey:
    case Name(name: String)
    case Pos(pos: Int)

  def apply[C](param: ConfigEvidence[C] ?=> Parameter[?, C], params: ConfigEvidence[C] ?=> Parameter[?, C]*): ParameterList[C] =
    given ConfigEvidence[C] = new ConfigEvidence[C]{}
     
    val resolvedParam = param
    val resolvedParams = params.map(param => param)
    new ParameterList[C](resolvedParam +: resolvedParams)

  extension [C](paramList: ParameterList[C])
    def collect(config: C, args: Seq[String]): (C, Position) = 
      collectParams(config, paramList, args)

  private enum Arg:
    case Named(name: String, value: String, nextPos: Position)
    case Flags(flags: String, value: String, nextPos: Position)
    case Value(value: String, nextPos: Position)
    case Separator(nextPos: Position)
    case End(nextPos: Position)

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
    new ParameterList(params :+ resolvedParam)

  @tailrec
  private def collectParams(
      args: Seq[String],
      positionalParams: Seq[Parameter[?, A]],
      namedParms: Map[String, Parameter[?, A]],
      actualParamms: Map[ParamKey, (Parameter[?, A], Seq[String])],
      pos: Position,
      positionalIndex: Int): (Map[ParamKey, (Parameter[?, A], Seq[String])], Position) =
    
    nextArg(namedParms, positionalParams, args, pos) match
      case Value(value, nextPos) =>
        if positionalIndex >= positionalParams.size then
          (actualParamms, pos) 
        else 
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
      case Named(name, value, nextPos) =>
        val updatedParams = actualParamms.updatedWith(ParamKey.Name(name)){
          case Some((param, values)) =>
            Some((param, values))
          case None => 
            val param = namedParms(name)
            Some((param, Seq(value)))
        }
        collectParams(args, positionalParams, namedParms, updatedParams , nextPos, positionalIndex)  
      
      case Separator(nextPos) =>
        collectParams(args, positionalParams, namedParms, actualParamms , nextPos, positionalIndex)

      case End(nextPos) =>
        (actualParamms, nextPos)    
  
  private def nextArg(namedParams: Map[String, Parameter[?, A]], positionalParams: Seq[Parameter[?, A]], args: Seq[String], pos: Position): Arg =
    val Position(argIndex, flagIndex) = pos

    if isEnd(args, pos) then 
      End(pos)

    else
      val arg = args(argIndex)
      if isSeparator(arg) then
        Separator(pos)

      if isName(arg) then
        namedParams.get(toName(arg)) match
          case Some(param) => 
            val name = param.name
            val valueSpec = param.valueSpec
            val valueIndex = argIndex + 1

            if valueIndex >= args.size then
              if valueSpec.defaultValue.isDefined then
                Named(name, valueSpec.defaultValue.get, Position(valueIndex, 0))
              else 
                throw ParameterException(
                  MissingValueException(), Position(valueIndex, 0))
            else
              val value = args(valueIndex)            
              if isDefinedName(value, namedParams.keySet) || isDefinedFlags(value, namedParams.keySet) then
                if valueSpec.defaultValue.isDefined then
                  Named(name, valueSpec.defaultValue.get, Position(valueIndex, 0))
                else 
                  throw ParameterException(
                    MissingValueException(), Position(valueIndex, 0))
              else
                if valueSpec.isValue(value) then
                  Named(name, value, Position(valueIndex+1, 0))
                else if valueSpec.defaultValue.isDefined then
                  Named(name, valueSpec.defaultValue.get, Position(valueIndex, 0))
                else
                  throw ParameterException(
                    IllegalValueException(), Position(valueIndex, 0))
          case None => End(pos)
       
        else if isDefinedFlags(arg, namedParams.keySet) then 
          End(pos)
        // else if isFlags(arg, namedParams.keySet) && arg.size > 1 && arg.size <= flagIndex+2 then
        //   nextArg(namedParams, positionalParams, args, Position(argIndex+1, 0), positionalIndex)

        // else if isFlags(arg, namedParams.keySet) && arg.size > 1 then
        //   val name = arg(flagIndex+1).toString
        //   val nextPos = if flagIndex+2 >= arg.size then Position(argIndex+1, 0) else Position(argIndex, flagIndex+1)
        //   namedParams.get(name) match
        //     case Some(param) => 
        //       Named(param.name, nextPos, positionalIndex)
        //     case None => End(pos)
        else 
          Value(arg, Position(argIndex+1, 0))

  private def isSeparator(arg: String): Boolean = arg == "--" || arg == "-"

  private def isShortName(arg: String): Boolean = 
       arg.size == 2
    && arg(0) == '-'
    && arg(1).isLetter

  private def isLongName(arg: String): Boolean =   
       arg.size > 2 
    && arg.startsWith("--") 
    && arg(2) != '-'

  private def isName(arg: String): Boolean = 
    isShortName(arg) || isLongName(arg)

  private def isDefinedName(arg: String, names: Set[String]): Boolean = 
       isName(arg)
    && names.contains(toName(arg))

  private def isFlags(arg: String): Boolean = 
       arg.size > 1 
    && arg.startsWith("-") 
    && arg(1) != '-' 

  private def toName(arg: String): String = 
    arg.stripPrefix("--").stripPrefix("-")

  private def isDefinedFlags(arg: String, names: Set[String]): Boolean = 
       isFlags(arg)
    && names.contains(arg(1).toString)
    
  private def isEnd(args: Seq[String], pos: Position): Boolean = 
    val Position(argIndex, flagIndex) = pos

    if argIndex >= args.size then true
    else if argIndex == args.size-1 && flagIndex + 1 >= args.last.size then true
    else false
