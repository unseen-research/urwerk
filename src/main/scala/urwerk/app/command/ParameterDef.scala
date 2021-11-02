package urwerk.app.command

import urwerk.source.Source

trait ParamOps[A]:
  type CC[_, _] >: Param[_, _]

  def param[B]: CC[A, B]

  def param[B](names: String): CC[A, B]

private type TLParam[C, V] = Param[C, V]

object Params:
  def apply[C1] = new Params[C1]{

    def onApply(commandTitle: String)(op: C1 => Source[Either[String, String]]): Cmd[C1] = ???

    def cmd(name: String): Cmd[C1] = ???

    def param[B]: Params.Param[C1, B] = ???

    def param[B](names: String): Params.Param[C1, B] = ???
  }

  trait Param[C, V] extends TLParam[C, V] with ParamOps[C]

trait Params[A] extends ParamOps[A]:
  def onApply(commandTitle: String)(op: A => Source[Either[String, String]]): Cmd[A]

  def cmd(name: String): Cmd[A]

trait Param[C, V]:
  val names: Seq[String]
  val arity: (Int, Int)
  val default: Option[V]

  //def onApply(op: (C, V) => C): Param[A] & Ctx[Config]

object Cmd:
  trait Param[C, V] extends TLParam[C, V] with Cmd[C]

trait Cmd[C] extends ParamOps[C]:
  def title: String
  def onApply(op: C => Source[Either[String, String]]): Cmd[C]

