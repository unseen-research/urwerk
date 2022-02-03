package urwerk.cli

import Parameter.ValueSpec
import ParameterList.ParameterSetting

trait WithConfig[C]:
  type CC = C

object Parameter:
  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?]): Parameter[V, config.CC] = 
    new Parameter(Seq(), valueSpec.defaultLabel, None, false, valueSpec, {(_, config) => config})

  def param[V](using valueSpec: ValueSpec[V], config: WithConfig[?])(name: String, names: String*): Parameter[V, config.CC] = 
    new Parameter(name +: names, valueSpec.defaultLabel, None, false, valueSpec, {(_, config) => config})

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
    val applyOp: (V, C) => C) extends ParameterSetting[V, C]:

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

  def required: Parameter[V, C] = copy(isRequired = true)

  def optional: Parameter[V, C] = copy(isRequired = false)
  
  def isOptional: Boolean = !isRequired 
