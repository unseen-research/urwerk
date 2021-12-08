package urwerk.app.cmd

object Parameter:
  type ConfigProvider[A] = () => A

class Parameter[A](val default: Option[A], val applyOp: A => _):
  import Parameter.*

  def default(value: A): Parameter[A] = Parameter[A](default = Some(value), applyOp)

  def onApply[B](op: (B, A) => B)(using config: ConfigProvider[B]): Parameter[A] = ???



type Param[A] = Parameter[A]