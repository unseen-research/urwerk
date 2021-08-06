package urwerk.command

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

    val pf: (A, B) => B = 
      case (value: A, config) => op(value, config)
    copy(collectOp = pf)

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

  class ParameterList[A](params: Seq[Parameter[?, A]]):
    def collectParams(args: Seq[String]): (A, Seq[String]) = ???     

class Parameters[A](config: A):

  def param[B](using valueTypeOps: ValueTypeOps[B]): Parameter[B, A] = new Parameter(Seq(), (0, 1), None, config, valueTypeOps, {(_, config) => config})
