package urwerk.cli

import scala.annotation.tailrec

import Parameter.ValueSpec
import ParameterList.Position

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
    def defaultValue: Option[V] = None
    def convert(value: String): V
    def convertSeq(values: Seq[String]): V
    def defaultLabel: String

  given ValueSpec[String] with
    def convert(value: String): String = value
    def convertSeq(values: Seq[String]): String = convert(values.last)
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    def convert(value: String): Int = value.toInt
    def convertSeq(values: Seq[String]): Int = convert(values.last)
    def defaultLabel: String = "INT"

  given ValueSpec[Boolean] with
    override def defaultValue: Option[Boolean] = Some(true)
    def convert(value: String): Boolean = 
      value.toLowerCase.toBoolean
      
    def convertSeq(values: Seq[String]): Boolean = convert(values.last)
    def defaultLabel: String = "BOOLEAN"



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

object ParameterList:

  case class Position(val argIndex: Int, val flagIndex: Int)

  def apply[C](param: ConfigEvidence[C] ?=> Parameter[?, C], params: ConfigEvidence[C] ?=> Parameter[?, C]*): ParameterList[C] =
    given ConfigEvidence[C] = new ConfigEvidence[C]{}
     
    val resolvedParam = param
    val resolvedParams = params.map(param => param)
    new ParameterList[C](resolvedParam +: resolvedParams)

  extension [C](paramList: ParameterList[C])
    def collect(config: C, args: Seq[String]): (C, Position) = 
      collectParams(config, paramList, args)

  private def collectParams[C](config: C, 
      paramList: ParameterList[C], 
      args: Seq[String]): (C, Position) =
    val params = paramList.params
    val positionalParams = positionalParameters(params)
    val namedParams = namedParameters(params) 
    
    collectParams(namedParams, positionalParams, config, args, Position(0, 0), "")

  @tailrec
  private def collectParams[C](namedParams: Map[String, Parameter[?, C]], 
      positionalParams: Seq[Parameter[?, C]], 
      config: C,
      args: Seq[String], 
      pos: Position, 
      previousName: String): (C, Position) =

    val Position(argIndex, flagIndex) = pos

    if argIndex >= args.size then
      val updatedConfig = if previousName.nonEmpty then
        namedParams.get(previousName) match
          case Some(param) =>
            param.valueSpec.defaultValue match
              case Some(defaultValue) =>
                param.applyOp(defaultValue, config)
              case None =>
                throw ParameterException(MissingValueException(), pos)            

          case None =>
            throw IllegalStateException("this position may never be reached")
      else config

      (updatedConfig, pos)
    
    else if isFlags(args(argIndex)) && flagIndex >= args(argIndex).size -1 then 
      collectParams(namedParams, positionalParams, config, args, Position(argIndex+1, 0), previousName)

    else
      val arg = args(argIndex)
      if isName(arg) then
        val updatedConfig = if previousName.nonEmpty then
          namedParams.get(previousName) match
            case Some(param) =>
              param.valueSpec.defaultValue match
                case Some(defaultValue) =>
                  param.applyOp(defaultValue, config)
                case None =>
                  throw ParameterException(MissingValueException(), pos)            

            case None =>
              throw IllegalStateException("this position may never be reached")
        else config

        val name = toName(arg)
        val paramOpt = namedParams.get(name) 
      
        if paramOpt.isDefined then   
          collectParams(namedParams, positionalParams, updatedConfig, args, Position(argIndex+1, 0), name)
      
        else
          (updatedConfig, pos)

      else if isFlags(arg) then 
        val updatedConfig = if previousName.nonEmpty then
          namedParams.get(previousName) match
            case Some(param) =>
              param.valueSpec.defaultValue match
                case Some(defaultValue) =>
                  param.applyOp(defaultValue, config)
                case None =>
                  throw ParameterException(MissingValueException(), pos)            

            case None =>
              throw IllegalStateException("this position may never be reached")
        else config
        
        val flags = arg.stripPrefix("-")
        val name = flags(flagIndex).toString
        val paramOpt = namedParams.get(name) 

        if paramOpt.isDefined then
          collectParams(namedParams, positionalParams, updatedConfig, args, Position(argIndex, flagIndex+1), name)
        else
          (updatedConfig, pos)
      
      else if isSeparator(arg) then
        val updatedConfig = if previousName.nonEmpty then
          namedParams.get(previousName) match
            case Some(param) =>
              param.valueSpec.defaultValue match
                case Some(defaultValue) =>
                  param.applyOp(defaultValue, config)
                case None =>
                  throw ParameterException(MissingValueException(), pos)            

            case None =>
              throw IllegalStateException("this position may never be reached")
        else config

        collectParams(namedParams, positionalParams, updatedConfig, args, Position(argIndex+1, 0), "")
      
      else 
        val value = stripQuotes(arg)
        if previousName.nonEmpty then
          namedParams.get(previousName) match
            case Some(param) =>
              val (updatedConfig, updatedPos) = try
                (param.applyOp(param.valueSpec.convert(value), config), Position(argIndex+1, 0))
              catch
                case _: IllegalArgumentException =>
                  param.valueSpec.defaultValue match
                    case Some(defaultValue) =>
                      (param.applyOp(defaultValue, config), Position(argIndex, 0))
                    case None =>
                      throw ParameterException(MissingValueException(), pos)  
                case e: Throwable => throw e

              collectParams(namedParams, positionalParams, updatedConfig, args, updatedPos, "")                
            case None =>
              throw IllegalStateException("this position may never be reached")
        else
          if positionalParams.size == 0 then
            (config, pos)
          else
            val param = positionalParams.head
            val updatedConfig = param.applyOp(param.valueSpec.convert(value), config)
            collectParams(namedParams, positionalParams.tail, updatedConfig, args, Position(argIndex+1, 0), "")    
    
  
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

  private[cli] def isSeparator(arg: String): Boolean = arg.count(_ == '-') == arg.size

  private[cli] def isName(arg: String): Boolean = 
    def isShortName: Boolean = 
        arg.size == 2
      && arg(0) == '-'
      && arg(1).isLetter

    def isLongName: Boolean =   
        arg.size > 2 
      && arg.startsWith("--") 
      && arg(2) != '-'

    isShortName || isLongName

  private[cli] def isFlags(arg: String): Boolean = 
       arg.size > 1 
    && arg.startsWith("-") 
    && arg(1).isLetter

  private[cli] def toName(arg: String): String = 
    arg.stripPrefix("--").stripPrefix("-")

  private [cli]def stripQuotes(value: String): String = 
    if value.startsWith("\"") && value.endsWith("\"") then
      value.stripPrefix("\"").stripSuffix("\"")
    else if value.startsWith("'") && value.endsWith("'") then
      value.stripPrefix("'").stripSuffix("'")
    else value

class ParameterList[C](val params: Seq[Parameter[?, C]]):
  def add(param: ConfigEvidence[C] ?=> Parameter[?, C], params: ConfigEvidence[C] ?=> Parameter[?, C]*): ParameterList[C] =
    given ConfigEvidence[C] = new ConfigEvidence[C]{}
     
    val resolvedParam = param
    val resolvedParams = params.map(param => param)

    new ParameterList[C](this.params ++ (resolvedParam +: resolvedParams))

