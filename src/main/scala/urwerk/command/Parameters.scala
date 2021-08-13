package urwerk.command

import scala.annotation.tailrec

object Parameter: 

  trait ValueSpec[A]:
    def requireValue: Boolean
    def accept(value: String): Boolean
    def convert(value: String): A

  given ValueSpec[String] with {
    val requireValue = true
    def accept(value: String): Boolean = 
      val retVal: Boolean = !value.startsWith("-")
      println(s"ACCEPT $value $retVal")
      retVal

    def convert(value: String): String = value
  }

  given ValueSpec[Int] with {
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("--")
    def convert(value: String): Int = value.toInt
  }

  given ValueSpec[Unit] with {
    val requireValue = false
    def accept(value: String): Boolean = value.isEmpty
    def convert(value: String): Unit = ()
  }

import Parameter.ValueSpec  

class Parameter[A, B](val names: Seq[String], 
    val arity: (Int, Int), 
    val default: Option[A], 
    val valueSpec: ValueSpec[A],
    val collectOp: (A, B) => B):
  
  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 
    copy(collectOp = op)

  private def copy(names: Seq[String] = names, 
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      valueSpec: ValueSpec[A] = valueSpec,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, arity, default, valueSpec, collectOp)
  
object Parameters:
  extension[B] (param: Parameter[?, B])
    private[command] def collectValue(config: B, value: String): B = 
      if !param.valueSpec.accept(value) then
        throw IllegalArgumentException()
      val _val = param.valueSpec.convert(value)
      param.collectOp(_val, config)

  class ParameterList[A](config: A, params: Seq[Parameter[?, A]]):
    
    def collectParams(args: Seq[String]): (A, Seq[String]) =
      val paramsMap = namedParamMap(params, Map())
      collectParams(args, positionalParamList, config, paramsMap)

    private def collectParams(args: Seq[String], positionalParams: Seq[Parameter[?, A]], config: A, paramsMap: Map[String, Parameter[?, A]]): (A, Seq[String]) = 
      if args.isEmpty then
        (config, args)
      else
        val arg = args.head
        if isName(arg) then
          val name = arg.stripPrefix("--")
          paramsMap.get(name) match
            case Some(param) =>
              val valueSpec = param.valueSpec
              val value = if valueSpec.requireValue then
                args(1)
              else 
                ""
              
              val _config = param.collectValue(config, value)  
              // if !valueSpec.accept(value) then

              //   throw IllegalArgumentException()

              // val _value = param.valueSpec.convert(value)
              // val _config = param.collectOp(_value, config)
              
              val remainingArgs = if valueSpec.requireValue then args.drop(2) else args.drop(1)
              collectParams(remainingArgs, positionalParams, _config, paramsMap)
            case None =>
              (config, args)
        else
          if positionalParams.isEmpty then
            (config, args)
          else
            val value = arg
            val param = positionalParams.head
            val valueSpec = param.valueSpec

            val _config = param.collectValue(config, value) 
            // if !valueSpec.accept(value) then
            //     throw IllegalArgumentException()

            // val _value = param.valueSpec.convert(arg)
            // val _config = param.collectOp(_value, config)

            collectParams(args.drop(1), positionalParams.drop(1), _config, paramsMap)

    private def isName(arg: String): Boolean = arg.startsWith("--")

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
    new Parameter(Seq(), (0, 1), None, valueSpec, {(_, config) => config})

  def param[B](using valueSpec: ValueSpec[B])(name: String, names: String*): Parameter[B, A] = 
    new Parameter(name +: names, (0, 1), None, valueSpec, {(_, config) => config})
