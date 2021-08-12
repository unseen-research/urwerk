package urwerk.command

import scala.annotation.tailrec

object Parameter: 

  trait ValueTypeOps[A]:
    def requireValue: Boolean
    def apply(value: String): A

  given ValueTypeOps[String] with {
    val requireValue = true
    def apply(value: String): String = 
      if value.startsWith("-") then
        throw new IllegalArgumentException()
      else
        value
  }

  given ValueTypeOps[Int] with {
    val requireValue = true
    def apply(value: String): Int = value.toInt
  }

  given ValueTypeOps[Unit] with {
    val requireValue = false
    def apply(value: String): Unit = ()
  }

  case class RawArg(value: String)

  given ValueTypeOps[RawArg] with {
    val requireValue = true
    def apply(value: String): RawArg = RawArg(value)
  }

import Parameter.ValueTypeOps  

class Parameter[A, B](val names: Seq[String], 
    val arity: (Int, Int), 
    val default: Option[A], 
    val config: B,
    val valueTypeOps: ValueTypeOps[A],
    val collectOp: (A, B) => B):
  
  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 
    copy(collectOp = op)

  private def copy(names: Seq[String] = names, 
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      config: B = config,
      valueTypeOps: ValueTypeOps[A] = valueTypeOps,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, arity, default, config, valueTypeOps, collectOp)
  
object Parameters:
  def apply[A](config: A): Parameters[A] = new Parameters[A](config)

  extension[A, B] (param: Parameter[A, B])
    private[command] def collectValue(value: String): B = 
      val _val = param.valueTypeOps(value)
      param.collectOp(_val, param.config)

  class ParameterList[A](config: A, params: Seq[Parameter[?, A]]):
    
    def collectParams(args: Seq[String]): (A, Seq[String]) =
      val paramsMap = namedParamMap(params, Map())
      collectParams(args, config, paramsMap)

    private def collectParams(args: Seq[String], config: A, paramsMap: Map[String, Parameter[?, A]]): (A, Seq[String]) = 
      if args.isEmpty then
        (config, args)
      else
        val arg = args.head
        if isName(arg) then
          val name = arg.stripPrefix("--")
          paramsMap.get(name) match
            case Some(param) =>
              val valueRequired = param.valueTypeOps.requireValue
              val value = if valueRequired then
                args(1)
              else 
                ""
                
              val _value = param.valueTypeOps(value)
              val _config = param.collectOp(_value, config)
              
              val remainingArgs = if valueRequired then args.drop(2) else args.drop(1)
              collectParams(remainingArgs, _config, paramsMap)
            case None =>
              (config, args)
        else
          
          (config, args)

    private def isName(arg: String): Boolean = arg.startsWith("--")

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

class Parameters[A](config: A):

  def param[B](using valueTypeOps: ValueTypeOps[B]): Parameter[B, A] = 
    new Parameter(Seq(), (0, 1), None, config, valueTypeOps, {(_, config) => config})

  def param[B](using valueTypeOps: ValueTypeOps[B])(name: String, names: String*): Parameter[B, A] = 
    new Parameter(name +: names, (0, 1), None, config, valueTypeOps, {(_, config) => config})
