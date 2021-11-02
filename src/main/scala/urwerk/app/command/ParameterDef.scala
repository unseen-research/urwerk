package urwerk.app.command

import urwerk.source.Source

trait ParamOps[A]:
  def param[B]: Params[A] with Param[B]

  def param[B](names: String): Params[A] with Param[B]

object Params:
  def apply[A] = new Params[A]{
    def onApply(commandTitle: String)(op: A => Source[Either[String, String]]): Cmd[A] = ???

    def cmd(name: String): Cmd[A] = ???

    def param[B]: Params[A] with Param[B] = ???

    def param[B](names: String): Params[A] with Param[B] = ???
  }


trait Params[A] extends ParamOps[A]:
  def onApply(commandTitle: String)(op: A => Source[Either[String, String]]): Cmd[A]

  def cmd(name: String): Cmd[A]

trait Param[A]:
  val names: Seq[String]
  val arity: (Int, Int)
  val default: Option[A]

  //def onApply(op: (Config, A) => Config): Param[A] & Ctx[Config]

trait Cmd[A] extends ParamOps[A]:
  def title: String
  def onApply(op: A => Source[Either[String, String]]): Cmd[A]

