package urwerk.command

object Parameter: 

  trait Converter[A]:
    def requireValue: Boolean
    def apply(value: String): A

  given Converter[String] with {
    val requireValue = true
    def apply(value: String): String = 
      if value.startsWith("-") then
        throw new IllegalArgumentException()
      else
        value
  }

  given Converter[Int] with {
    val requireValue = true
    def apply(value: String): Int = value.toInt
  }

  given Converter[Unit] with {
    val requireValue = false
    def apply(value: String): Unit = ()
  }

  case class RawArg(value: String)

  given Converter[RawArg] with {
    val requireValue = true
    def apply(value: String): RawArg = RawArg(value)
  }

import Parameter.Converter  

class Parameter[A, B](val names: Seq[String], 
    val arity: (Int, Int), 
    val default: Option[A], 
    val config: B,
    val converter: Parameter.Converter[A],
    val collectOp: (A, B) => B):
  
  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 

    val pf: (A, B) => B = 
      case (value: A, config) => op(value, config)
    copy(collectOp = pf)

  private[command] def doCollect(value: String): B = 
    val _val = converter(value)
    collectOp(_val, config)

  private def copy(names: Seq[String] = names, 
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      config: B = config,
      converter: Converter[A] = converter,
      collectOp: (A, B) => B = collectOp) = 
    new Parameter(names, arity, default, config, converter, collectOp)
  
object Parameters:
  def apply[A](config: A): Parameters[A] = new Parameters[A](config)

class Parameters[A](config: A):

  def param[B](using converter: Parameter.Converter[B]): Parameter[B, A] = new Parameter(Seq(), (0, 1), None, config, converter, {(_, config) => config})
