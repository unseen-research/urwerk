package urwerk.cli

import Parameter.ValueSpec
import ParameterList.ParameterSetting

trait WithConfig[C]:
  type CC = C

object Parameter:
  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?]): PositionalParameter[V, config.CC] = 
    new PositionalParameter(valueSpec.defaultLabel, None, false, valueSpec, {(config, _) => config}, _ => true)

  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?])(name: String, names: String*): NamedParameter[V, config.CC] = 
    new NamedParameter(name +: names, valueSpec.defaultLabel, None, false, valueSpec, {(config, _) => config}, _ => true)

  def trailingArgs[V](using config: WithConfig[?]): TrailingArgs[config.CC] = 
    val valueSpec = summon[ValueSpec[String]]
    new TrailingArgs(valueSpec.defaultLabel, None, false, valueSpec, {(config, _) => config}, _ => true)

  trait ValueSpec[V]:
    type VV = V
    def defaultValue: Option[V] = None
    def convert(value: String): V
    def defaultLabel: String

  given ValueSpec[String] with
    def convert(value: String): String = value
    def defaultLabel: String = "STRING"

  given ValueSpec[Int] with
    def convert(value: String): Int = value.toInt
    def defaultLabel: String = "INT"

  given ValueSpec[Boolean] with
    override def defaultValue: Option[Boolean] = Some(true)
    def convert(value: String): Boolean = 
      value.toLowerCase.toBoolean
    def defaultLabel: String = "BOOLEAN"

  extension(value: String)
    def toParam[C](using  WithConfig[C]): PositionalParameter[String, C] = 
      param.accept(arg => arg == value)

    def toParameter[C](using  WithConfig[C]): PositionalParameter[String, C] = toParam

  def isSeparator(arg: String): Boolean = arg.count(_ == '-') == arg.size

  def isName(arg: String): Boolean = 
    def isShortName: Boolean = 
        arg.size == 2
      && arg(0) == '-'
      && arg(1).isLetter

    def isLongName: Boolean =   
        arg.size > 2 
      && arg.startsWith("--") 
      && arg(2) != '-'

    isShortName || isLongName

  def isFlags(arg: String): Boolean = 
       arg.size > 1 
    && arg.startsWith("-") 
    && arg(1).isLetter

  def toName(arg: String): String = 
    arg.stripPrefix("--").stripPrefix("-")

  def stripQuotes(value: String): String = 
    if value.startsWith("\"") && value.endsWith("\"") then
      value.stripPrefix("\"").stripSuffix("\"")
    else if value.startsWith("'") && value.endsWith("'") then
      value.stripPrefix("'").stripSuffix("'")
    else value

sealed trait Parameter[V, C] extends ParameterSetting[V, C]:
  def names: Seq[String]
  def name: String    
  def label: String
  def default: Option[V]
  def isRequired: Boolean

  protected[cli] def valueSpec: ValueSpec[V]
  protected[cli] def applyOp: (C, V) => C
  protected[cli] def acceptOp: String => Boolean

case class NamedParameter[V, C](
      names: Seq[String],
      label: String,
      default: Option[V],
      isRequired: Boolean,
      valueSpec: ValueSpec[V],
      applyOp: (C, V) => C,
      acceptOp: String => Boolean) 
    extends Parameter[V, C]:

  def default(value: V): NamedParameter[V, C] = copy(default = Some(value))

  def apply(op: (C, V) => C): NamedParameter[V, C] =
    copy(applyOp = op)

  def accept(op: String => Boolean): NamedParameter[V, C] = 
    copy(acceptOp = op)

  def label(label: String): NamedParameter[V, C] =
    copy(label = label)

  def convert(value: String): V = valueSpec.convert(value)

  def name: String = names.headOption.getOrElse("")

  def name(name: String): NamedParameter[V, C] =
    copy(names = name +: names.drop(1))

  def names(name: String, names: String*): NamedParameter[V, C] =
    copy(names = name +: names)

  def required: NamedParameter[V, C] = copy(isRequired = true)

  def optional: NamedParameter[V, C] = copy(isRequired = false)
  
  def isOptional: Boolean = !isRequired 

case class PositionalParameter[V, C](
    label: String,
    default: Option[V],
    isRequired: Boolean,
    valueSpec: ValueSpec[V],
    applyOp: (C, V) => C,
    acceptOp: String => Boolean) extends Parameter[V, C]:

  def names: Seq[String] = Seq()
  
  def name: String = ""
  
  def default(value: V): PositionalParameter[V, C] = copy(default = Some(value))

  def apply(op: (C, V) => C): PositionalParameter[V, C] =
    copy(applyOp = op)

  def accept(op: String => Boolean): PositionalParameter[V, C] = 
    copy(acceptOp = op)

  def label(label: String): PositionalParameter[V, C] =
    copy(label = label)

  def convert(value: String): V = valueSpec.convert(value)

  def required: PositionalParameter[V, C] = copy(isRequired = true)

  def optional: PositionalParameter[V, C] = copy(isRequired = false)
  
  def isOptional: Boolean = !isRequired 

case class TrailingArgs[C](
    label: String,
    default: Option[String],
    isRequired: Boolean,
    valueSpec: ValueSpec[String],
    applyOp: (C, String) => C,
    acceptOp: String => Boolean) extends Parameter[String, C]:

  def names: Seq[String] = Seq()
  
  def name: String = ""
  
  def default(value: String): TrailingArgs[C] = copy(default = Some(value))

  def apply(op: (C, String) => C): TrailingArgs[C] =
    copy(applyOp = op)

  def accept(op: String => Boolean): TrailingArgs[C] = 
    copy(acceptOp = op)

  def label(label: String): TrailingArgs[C] =
    copy(label = label)

  def convert(value: String): String = valueSpec.convert(value)

  def required: TrailingArgs[C] = copy(isRequired = true)

  def optional: TrailingArgs[C] = copy(isRequired = false)
  
  def isOptional: Boolean = !isRequired 

