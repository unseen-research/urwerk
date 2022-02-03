package urwerk.cli

import Parameter.ValueSpec
import ParameterList.ParameterSetting

trait WithConfig[C]:
  type CC = C

object Parameter:
  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?]): Parameter[V, config.CC] = 
    new Parameter(Seq(), valueSpec.defaultLabel, None, false, valueSpec, {(_, config) => config}, _ => true)

  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?])(name: String, names: String*): Parameter[V, config.CC] = 
    new Parameter(name +: names, valueSpec.defaultLabel, None, false, valueSpec, {(_, config) => config}, _ => true)

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
    def toParam[C](using  WithConfig[C]): Parameter[String, C] = 
      param.accept(arg => arg == value)
      
    def toParameter[C](using  WithConfig[C]): Parameter[String, C] = toParam

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

case class Parameter[V, C](val names: Seq[String],
    val label: String,
    val default: Option[V],
    val isRequired: Boolean,
    val valueSpec: ValueSpec[V],
    val applyOp: (V, C) => C,
    val acceptOp: String => Boolean) extends ParameterSetting[V, C]:

  def default(value: V): Parameter[V, C] = copy(default = Some(value))

  def apply(op: (V, C) => C): Parameter[V, C] =
    copy(applyOp = op)

  def accept(op: String => Boolean): Parameter[V, C] = 
    copy(acceptOp = op)

  def label(label: String): Parameter[V, C] =
    copy(label = label)

  def convert(value: String): V = valueSpec.convert(value)

  def name: String = names.headOption.getOrElse("")

  def name(name: String): Parameter[V, C] =
    copy(names = name +: names.drop(1))

  def names(name: String, names: String*): Parameter[V, C] =
    copy(names = name +: names)

  def required: Parameter[V, C] = copy(isRequired = true)

  def optional: Parameter[V, C] = copy(isRequired = false)
  
  def isOptional: Boolean = !isRequired 
