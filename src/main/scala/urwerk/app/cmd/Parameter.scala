package urwerk.app.cmd

object Parameter:
  type ConfigProvider[A] = () => A

  def apply[A](): Parameter[A] = new Parameter(None, (x: A) => ???)

class Parameter[A](val default: Option[A], private val applyOp: A => _):
  import Parameter.*

  def default(value: A): Parameter[A] = new Parameter[A](default = Some(value), applyOp)

  def onApply[B](op: (B, A) => B)(using config: ConfigProvider[B]): Parameter[A] =
    val _applyOp = (value: A) => op(config(), value)
    new Parameter(default = default, applyOp = _applyOp)

  def apply[B](value: A): B = applyOp(value).asInstanceOf[B]

end Parameter

type Param[A] = Parameter[A]