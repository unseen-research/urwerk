package urwerk.command

import scala.annotation.tailrec

object Parameter: 
  trait ValueSpec[A]:
    def requireValue: Boolean
    def accept(value: String): Boolean
    def convert(value: String): A
    def defaultLabel: String

  given ValueSpec[String] with {
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("-")
    def convert(value: String): String = value
    def defaultLabel: String = "STRING"
  }

  given ValueSpec[Int] with {
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("--")
    def convert(value: String): Int = value.toInt
    def defaultLabel: String = "INT"
  }

  given ValueSpec[Unit] with {
    val requireValue = false
    def accept(value: String): Boolean = value.isEmpty
    def convert(value: String): Unit = ()
    def defaultLabel: String = "UNIT"
  }

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

  def accept(op: String => Boolean): Parameter[A, B] = 
    copy(acceptOp = op)
    
  def arity(minArity: Int, maxArity: Int): Parameter[A, B] = 
    copy(arity = (minArity, maxArity))

  def minArity: Int = arity._1

  def maxArity: Int = arity._2
  
  def label(label: String): Parameter[A, B] =
    copy(label = label)
    
  def name: String = names.headOption.getOrElse("")

  private def copy(names: Seq[String] = names,
      label: String = label,
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      valueRequired: Boolean = valueRequired,
      acceptOp: String => Boolean = acceptOp,
      convertOp: String => A = convertOp,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, label, arity, default, valueRequired, acceptOp, convertOp, collectOp)
  
object Parameters:

  class MissingParameterException(val labelOrName: String, val requiredArity: Int, val actualArity: Int) extends RuntimeException

  class ArityExceededException(val name: String, val maxArity: Int) extends RuntimeException

  extension[B] (param: Parameter[?, B])
    private[command] def collectValue(config: B, value: String): B = 
      if !param.acceptOp(value) then
        throw IllegalArgumentException()
      val _val = param.convertOp(value)
      param.collectOp(_val, config)

  object ParameterList:
    enum ParamKey:
      case Name(name: String)
      case Pos(pos: Int)

  class ParameterList[A](params: Seq[Parameter[?, A]]):
    import ParameterList.*
    import Parameters.*

    def collectParams(config: A, args: Seq[String]): (A, Seq[String]) =
      val paramsMap = namedParamMap(params, Map())

      val paramArities = {
        val posMap = positionalParamList
          .zipWithIndex.map((p, i)=> (ParamKey.Pos(i), (p, 0)))
          .toMap
        val nameMap = paramsMap
          .map((n, p) => (ParamKey.Name(n), (p, 0)))
        posMap ++ nameMap
      }

      collectParams(args, positionalParamList, config, paramsMap, paramArities)

    private def validateArities(arities: Map[ParamKey, (Parameter[?, A], Int)]) =
      arities.values.foreach{case (param, actualArity) =>
        val requiredArity = param.minArity
        val labelOrName = if param.name.nonEmpty then param.name else param.label
        if actualArity < requiredArity then
          throw MissingParameterException(labelOrName, requiredArity=requiredArity, actualArity=actualArity)
      }

    @tailrec
    private def collectParams(args: Seq[String], 
        positionalParams: Seq[Parameter[?, A]], 
        config: A, 
        paramsMap: Map[String, Parameter[?, A]],
        arities: Map[ParamKey, (Parameter[?, A], Int)]): (A, Seq[String]) =
      if args.isEmpty then
        validateArities(arities)
        (config, args)
      else
        val arg = args.head
        definedName(paramsMap, arg) match
          case "" =>
            if positionalParams.isEmpty then
              validateArities(arities)
              (config, args)
            else
              val value = arg
              val pos = this.positionalParamList.size - positionalParams.size
              val param = positionalParams.head
              val (minArity, maxArity) = param.arity
              
              val (_, arity) = arities(ParamKey.Pos(pos))

              if !param.acceptOp(value) then
                validateArities(arities)
                (config, args)
              else if arity + 1 >= maxArity then
                val _config = param.collectValue(config, value) 
                val _arities = arities
                  .updatedWith(ParamKey.Pos(pos)){case Some((param, arity)) => 
                    Some((param, arity + 1))}
                collectParams(args.drop(1), positionalParams.drop(1), _config, paramsMap, _arities)
              else
                val _config = param.collectValue(config, value) 
                val _arities = arities
                  .updatedWith(ParamKey.Pos(pos)){case Some((param, arity)) => 
                    Some((param, arity + 1))}              
                collectParams(args.drop(1), positionalParams, _config, paramsMap, _arities)

          case name if name.size == 1 =>
            ???
          case name =>
            val name = arg.stripPrefix("--")
            paramsMap.get(name) match
              case Some(param) =>
                val valueRequired = param.valueRequired
                val value = if valueRequired then
                  args(1)
                else 
                  ""
                
                val _config = param.collectValue(config, value)  
                val remainingArgs = if valueRequired then args.drop(2) else args.drop(1)
                val primaryName = param.name
                val _arities = arities
                  .updatedWith(ParamKey.Name(primaryName)){case Some((param, arity)) =>
                    val newArity = arity + 1
                    if newArity > param.maxArity then
                      throw ArityExceededException(name, param.maxArity)
                    Some((param, newArity))
                  }

                collectParams(remainingArgs, positionalParams, _config, paramsMap, _arities)
              case None =>
                validateArities(arities)
                (config, args)            

    private def definedName(paramsMap: Map[String, Parameter[?, A]], arg: String): String = 
      val name = if arg.startsWith("--") then
        arg.stripPrefix("--")
      else if arg.startsWith("-") && arg.size > 1 then
        arg(1).toString
      else ""
      if paramsMap.isDefinedAt(name) then name else ""
      
    private def isDefinedName(paramsMap: Map[String, Parameter[?, A]], arg: String): Boolean = 
      if arg.startsWith("--") then
        val name = arg.stripPrefix("--")
        paramsMap.isDefinedAt(name)
      else
        false

    private def isDefinedChar(paramsMap: Map[String, Parameter[?, A]], arg: String): Boolean = 
      if arg.indexWhere(_ != '-') == 1 && arg.size > 1 then
        val name = arg(1).toString
        paramsMap.isDefinedAt(name)
      else
        false

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

  def param[B](using valueSpec: ValueSpec[B]): Parameter[B, A] = 
    new Parameter(Seq(), valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})

  def param[B](using valueSpec: ValueSpec[B])(name: String, names: String*): Parameter[B, A] = 
    new Parameter(name +: names, valueSpec.defaultLabel, (0, 1), None, valueSpec, {(_, config) => config})
