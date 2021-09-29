package urwerk.app.command

import urwerk.source.Source
import urwerk.io.ByteString

object Cmd {
  enum Output:
    val output: ByteString
    case Standard(output: ByteString) extends Output
    case Error(output: ByteString) extends Output
}

trait Cmd[A]:  
  def name: String
  def description(text: String): Command[A]
  def apply(op: A => Source[Cmd.Output]): Command[A]
  def resolve(args: Seq[String]): Source[Cmd.Output]