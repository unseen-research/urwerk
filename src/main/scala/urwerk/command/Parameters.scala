package urwerk.command

object Parameter: 
  trait Converter[A]:
    def convert(value: String): A

  given Converter[String] with {
    def convert(value: String): String = value
  }

import Parameter.Converter  

class Parameter[A, B](val names: Seq[String], 
    val arity: (Int, Int), 
    val default: Option[A], 
    val config: B,
    val collectOp: PartialFunction[(A, B), B],
    val converter: Parameter.Converter[A]):
  
  def default(value: A): Parameter[A, B] = copy(default = Some(value))

  def collect(op: (A, B) => B): Parameter[A, B] = 
    copy(collectOp = {
      case (value, config) => op(value, config)})

  def doCollect(value: String): B = 
    val _val = converter.convert(value)
    collectOp(_val, config)

  private def copy(names: Seq[String] = names, 
      arity: (Int, Int) = arity, 
      default: Option[A] = default, 
      config: B = config,
      collectOp: PartialFunction[(A, B), B] = collectOp, 
      converter: Converter[_ >: A] = converter) = 
    new Parameter(names, arity, default, config, collectOp, converter.asInstanceOf[Converter[A]])
  
object Parameters:
  def apply[A](config: A): Parameters[A] = new Parameters[A](config)

class Parameters[A](config: A):
  
  def param[B](using converter: Parameter.Converter[B]): Parameter[B, A] = new Parameter(Seq(), (0, 1), None, config, PartialFunction.empty, converter)
