package urwerk.command

import scala.annotation.tailrec

object Parameter: 

  trait ValueSpec[A]:
    def requireValue: Boolean
    def accept(value: String): Boolean
    def convert(value: String): A

  given ValueSpec[String] with {
    val requireValue = true
    def accept(value: String): Boolean = !value.startsWith("-")
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
    val valueRequired: Boolean,
    val acceptOp: String => Boolean,
    val convertOp: String => A,
    val collectOp: (A, B) => B):
  
  def this(names: Seq[String], 
      arity: (Int, Int), 
      default: Option[A], 
      valueSpec: ValueSpec[A],
      collectOp: (A, B) => B) =

    this(names, arity, default, valueSpec.requireValue, valueSpec.accept, valueSpec.convert, collectOp)    

  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 
    copy(collectOp = op)

  def accept(op: String => Boolean): Parameter[A, B] = 
    copy(acceptOp = op)
    
  def arity(minArity: Int, maxArity: Int): Parameter[A, B] = 
    copy(arity = (minArity, maxArity))

  private def copy(names: Seq[String] = names, 
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      valueRequired: Boolean = valueRequired,
      acceptOp: String => Boolean = acceptOp,
      convertOp: String => A = convertOp,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, arity, default, valueRequired, acceptOp, convertOp, collectOp)
  
object Parameters:
  extension[B] (param: Parameter[?, B])
    private[command] def collectValue(config: B, value: String): B = 
      if !param.acceptOp(value) then
        throw IllegalArgumentException()
      val _val = param.convertOp(value)
      param.collectOp(_val, config)

  class ParameterList[A](params: Seq[Parameter[?, A]]):
    
    def collectParams(config: A, args: Seq[String]): (A, Seq[String]) =
      val paramsMap = namedParamMap(params, Map())
      collectParams(args, positionalParamList, config, paramsMap, 0, Map())

    private def collectParams(args: Seq[String], 
        positionalParams: Seq[Parameter[?, A]], 
        config: A, 
        paramsMap: Map[String, Parameter[?, A]],
        positionalArity: Int,
        paramArity: Map[String, Int]): (A, Seq[String]) = 
      if args.isEmpty then
        (config, args)
      else
        val arg = args.head
        if isDefinedName(paramsMap, arg) then
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
              val _paramArity = paramArity.updatedWith(name)(_.map(_ + 1).orElse(Some(0)))
              collectParams(remainingArgs, positionalParams, _config, paramsMap, positionalArity, _paramArity)
            case None =>
              (config, args)
        else
          if positionalParams.isEmpty then
            (config, args)
          else
            val value = arg
            val param = positionalParams.head
            val (minArity, maxArity) = param.arity

            if !param.acceptOp(value) then
              (config, args)
            else if positionalArity +1 >= maxArity then 
              val _config = param.collectValue(config, value) 
              collectParams(args.drop(1), positionalParams.drop(1), _config, paramsMap, positionalArity + 1, paramArity)
            else
              val _config = param.collectValue(config, value) 
              collectParams(args.drop(1), positionalParams, _config, paramsMap, positionalArity + 1, paramArity)

    private def isDefinedName(paramsMap: Map[String, Parameter[?, A]], arg: String): Boolean = 
      if arg.startsWith("--") then
        val name = arg.stripPrefix("--")
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
    new Parameter(Seq(), (0, 1), None, valueSpec, {(_, config) => config})

  def param[B](using valueSpec: ValueSpec[B])(name: String, names: String*): Parameter[B, A] = 
    new Parameter(name +: names, (0, 1), None, valueSpec, {(_, config) => config})
