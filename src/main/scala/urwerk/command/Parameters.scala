package urwerk.command

import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.util.Try
import scala.util.Failure
import urwerk.command.Parameters.CollectValueException

object Parameter: 
  trait ValueSpec[A]:
    def requireValue: Boolean
    def accept(value: String): Boolean
    def convert(value: String): A
    def defaultLabel: String

  given ValueSpec[String] with 
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("-")
    def convert(value: String): String = value
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("--")
    def convert(value: String): Int = value.toInt
    def defaultLabel: String = "INT"

  given ValueSpec[Unit] with
    val requireValue = false
    def accept(value: String): Boolean = value.isEmpty
    def convert(value: String): Unit = ()
    def defaultLabel: String = "UNIT"

import Parameter.ValueSpec  

class Parameter[A, B](val names: Seq[String],
    val label: String,
    val arity: (Int, Int), 
    val default: Option[A], 
    val valueRequired: Boolean,
    val acceptOp: String => Boolean,
    val convertOp: String => A,
    val collectOp: (A, B) => B):
  
  def this(names: Seq[String],
      label: String,
      arity: (Int, Int), 
      default: Option[A], 
      valueSpec: ValueSpec[A],
      collectOp: (A, B) => B) =
    this(names, label, arity, default, valueSpec.requireValue, valueSpec.accept, valueSpec.convert, collectOp)

  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 
    copy(collectOp = op)

  def apply(op: (A, B) => B): Parameter[A, B] = 
    collect(op)

  def accept(op: String => Boolean): Parameter[A, B] = 
    copy(acceptOp = op)
    
  def arity(minArity: Int, maxArity: Int): Parameter[A, B] = 
    copy(arity = (minArity, maxArity))

  def minArity: Int = arity._1

  def maxArity: Int = arity._2
  
  def label(label: String): Parameter[A, B] =
    copy(label = label)
    
  def name: String = names.headOption.getOrElse("")

  def name(name: String): Parameter[A, B] =
    copy(names = name +: names.drop(1))

  def names(name: String, names: String*): Parameter[A, B] =
    copy(names = name +: names)

  private def copy(names: Seq[String] = names,
      label: String = label,
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      valueRequired: Boolean = valueRequired,
      acceptOp: String => Boolean = acceptOp,
      convertOp: String => A = convertOp,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, label, arity, default, valueRequired, acceptOp, convertOp, collectOp)
  
  private[command] def collectValue(config: B, value: String): B = 
    if !acceptOp(value) then
      throw IllegalArgumentException()
    val _val = convertOp(value)
    applyCollectOp(_val, config)

  private[command] def collectDefault(config: B): B = 
    default.map(value => applyCollectOp(value, config))
      .getOrElse(config)

  private def applyCollectOp(value: A, config: B): B = 
    Try(
        collectOp(value, config))
      .recoverWith{case ex: Throwable => Failure(CollectValueException(ex))}
      .get

object Parameters:

  class ParameterException(message: String, cause: Throwable) extends RuntimeException(message, cause):
    def this() = this("", null)
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this("message", cause)

  class ArityExceededException(val name: String, val maxArity: Int) extends ParameterException

  class MissingParameterException(val labelOrName: String, val requiredArity: Int, val repetition: Int) extends ParameterException

  class NoSuchValueException extends ParameterException

  class CollectValueException(cause: Throwable) extends ParameterException(cause)

  object ParameterList:
    enum ParamKey:
      case Name(name: String)
      case Pos(pos: Int)

  class ParameterList[A](params: Seq[Parameter[?, A]]):
    import ParameterList.*
    import Parameters.*

    def collectParams(config: A, args: Seq[String]): (A, Int, Int) = 
      collectParams(config, args, 0, 0)

    def collectParams(config: A, args: Seq[String], argIndex: Int, flagIndex: Int): (A, Int, Int) =
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

      collectParams(args, argIndex, flagIndex, positionalParamList, config, paramsMap, paramRepetions)

    private def postProcess(config: A, repetitions: Map[ParamKey, (Parameter[?, A], Int)]): A = 
      val (_config, _repetitions) = applyDefaults(config, repetitions)
      validateArity(_repetitions)
      _config

    private def applyDefaults(config: A, repetitions: Map[ParamKey, (Parameter[?, A], Int)]): (A, Map[ParamKey, (Parameter[?, A], Int)]) = 
      repetitions.foldLeft((config, repetitions)){case ((config, repetitions), (paramKey, (param, repetition))) =>
        if repetition == 0 && param.default.isDefined then
          (param.collectDefault(config), 
            repetitions.updated(paramKey, (param, 1)))
        else 
          (config, repetitions)
      }

    private def validateArity(repetitions: Map[ParamKey, (Parameter[?, A], Int)]) =
      repetitions.values.foreach{case (param, repetition) =>
        val requiredArity = param.minArity
        val labelOrName = if param.name.nonEmpty then param.name else param.label
        if repetition < requiredArity then
          throw MissingParameterException(labelOrName, requiredArity = requiredArity, repetition = repetition)
      }

    @tailrec
    private def collectParams(args: Seq[String], 
        argIndex: Int,
        flagIndex: Int,
        positionalParams: Seq[Parameter[?, A]], 
        config: A, 
        paramsMap: Map[String, Parameter[?, A]],
        repetitions: Map[ParamKey, (Parameter[?, A], Int)]): (A, Int, Int) =
      if argIndex >= args.size then
        val _config = postProcess(config, repetitions)
        (_config, argIndex, flagIndex)
      else
        val arg = args(argIndex)
        definedName(paramsMap, arg, flagIndex) match
          case ("", _) if positionalParams.isEmpty =>
              //postProcess(config, args, repetitions)
            val _config = postProcess(config, repetitions)
            (_config, argIndex, flagIndex)
          case ("", _) =>
            val value = arg
            val pos = positionalParamList.size - positionalParams.size
            val param = positionalParams.head
            val (minArity, maxArity) = param.arity
            
            val (_, arity) = repetitions(ParamKey.Pos(pos))

            if !param.acceptOp(value) then
              //postProcess(config, args, repetitions)
              val _config = postProcess(config, repetitions)
              (_config, argIndex, flagIndex)
            else if arity + 1 >= maxArity then
              val _config = param.collectValue(config, value) 
              val _repetitions = repetitions
                .updatedWith(ParamKey.Pos(pos)){case Some((param, arity)) => 
                  Some((param, arity + 1))}
              collectParams(args, argIndex + 1, 0, positionalParams.drop(1), _config, paramsMap, _repetitions)
            else
              val _config = param.collectValue(config, value) 
              val _repetitions = repetitions
                .updatedWith(ParamKey.Pos(pos)){case Some((param, arity)) => 
                  Some((param, arity + 1))}              
              collectParams(args, argIndex+1, 0, positionalParams, _config, paramsMap, _repetitions)

          case (name, nextFlagIndex) =>
            paramsMap.get(name) match
              case Some(param) =>
                val valueRequired = param.valueRequired

                if valueRequired && name.size == 1 && flagIndex +2 < arg.size then 
                  //"-aAbBc", "valueC", "tail"
                  println(s"FLAGSSS $name $flagIndex $valueRequired")
                  throw NoSuchValueException()

                val value = if valueRequired then
                  args(argIndex + 1)
                else ""
                
                val _config = param.collectValue(config, value)  
                
                val primaryName = param.name
                val _repetitions = repetitions
                  .updatedWith(ParamKey.Name(primaryName)){case Some((param, arity)) =>
                    val newArity = arity + 1
                    if newArity > param.maxArity then
                      throw ArityExceededException(name, param.maxArity)
                    Some((param, newArity))
                  }

                val nextArgIndex = if nextFlagIndex > 0 then argIndex
                else if value.nonEmpty then argIndex + 2
                else argIndex + 1
                collectParams(args, nextArgIndex, nextFlagIndex, positionalParams, _config, paramsMap, _repetitions)
              case None =>
                //postProcess(config, args, repetitions)          
                val _config = postProcess(config, repetitions)
                (_config, argIndex, flagIndex)  

    private def definedName(paramsMap: Map[String, Parameter[?, A]], arg: String, flagIndex: Int): (String, Int) = 
      val (name, nextFlagIndex) = if arg.startsWith("--") then
        (arg.stripPrefix("--"), 0)
      else if arg.startsWith("-") && arg.size > 1 then
        val nextFlagIndex = if flagIndex + 2 >= arg.size then 0 else flagIndex + 1
        (arg(flagIndex+1).toString, nextFlagIndex)
      else ("", 0)

      println(s"NEXTFLAG  $arg $flagIndex $nextFlagIndex $name")
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

class Parameters[A]():

  inline def param[B](using valueSpec: ValueSpec[B]): Parameter[B, A] = 
    new Parameter(Seq(), valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})

  inline def param[B](using valueSpec: ValueSpec[B])(name: String, names: String*): Parameter[B, A] = 
    new Parameter(name +: names, valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})
