package urwerk.main

import urwerk.source.Source
import urwerk.source.Optional

type Output = ErrOut | StdOut

case class ErrOut(value: String) extends AnyVal

case class StdOut(value: String) extends AnyVal

type Command = Seq[String] => Source[Either[String, String]]
  
trait ExitStatus:  
  def exitStatus: Int 

type ExitOp = Int => Unit


class Main(command: Command, onError: Throwable => Unit, exitOp: ExitOp):
  def main(args: Array[String]): Unit =
    val cmd: Source[Either[String, String]] = try
      command(args.toSeq)
    catch
      case error: Throwable =>
        onError(error)
        exitOp(-1)
        Source()

    val errorOpt = cmd.doOnNext{
      case Right(out) =>
        Console.out.print(out)
      case Left(out) =>
        Console.err.print(out)
    }
    .filter(_ => false)
    .map[Throwable](_ => Throwable())
    .onErrorResume(error => 
      Source(error))
    .lastOption
    .block
        
    errorOpt match
      case Some(error: ExitStatus) =>
        onError(error)
        exitOp(error.exitStatus)
      case Some(error) =>
        onError(error)
        exitOp(-1)
      case None =>
        ()