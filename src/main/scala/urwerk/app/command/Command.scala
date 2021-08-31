package urwerk.app.command

import urwerk.app.command.Parameters.ParameterList
import urwerk.app.command.Parameters.Position
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Command: 
  import Command as CommandIf

  def apply[A](name: String, config: A): CommandParameterList[A] = new CommandParameterList[A](new Command[A](name, config, Seq(), ""){}, config, Seq())
   
  class CommandParameterList[A](val innerCmd: Command[A], config: A, parameterLists: Seq[ParameterList[A]]) extends CommandIf[A]:
    def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = ???
      new CommandParameterList[A](innerCmd, config, parameterLists :+ ParameterList(param +: params))

    export innerCmd.*

  trait Command[A](val name: String, config: A, parameterLists: Seq[ParameterList[A]], description: String) extends CommandIf[A]:
    def description(text: String): Command[A] = copy(description = description)

    def apply(args: Seq[String]): Try[A] = 
      collectParams(parameterLists, config, args, Position(0, 0))

    @tailrec
    private def collectParams(paramLists: Seq[ParameterList[A]], config: A, args: Seq[String], pos: Position): Try[A] =
      if paramLists.isEmpty then
        Success(config)
      else
        val paramList = paramLists.head
        val res = Try(
            paramList.collectParams(config, args, pos))

        res match 
          case Success(_config, _pos) =>
            collectParams(paramLists.drop(1), _config, args, _pos)
          case f: Failure[?] =>  
            f.asInstanceOf[Failure[A]]

    private[command] def copy(
        name: String = name,
        config: A = config,
        parameterLists: Seq[ParameterList[A]] = parameterLists,
        description: String = description) = 
      new Command[A](name, config, parameterLists, description){}


trait Command[A]:  
  def description(text: String): Command[A]
  def apply(args: Seq[String]): Try[A]

trait CommandParameterList[A] extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A]

trait CommandFeed[A]: 
  def feed(commands: Seq[Command[A]]): Command[A]

object Commands:
  def apply[A](command: Command[A]*): Commands[A] = 
    new Commands(command, a => ???) 
    
class Commands[A](commands: Seq[Command[A]], onErrorOp: Seq[(Command[A], Throwable)] => A):
  def onError(op: Seq[(Command[A], Throwable)] => A): Commands[A] = 
    new Commands[A](commands, op)

  def resolve(args: Seq[String]): A = 
    resolve(args, commands, Seq()) match 
      case Left(errors) =>
        onErrorOp(errors)
      case Right(config) => config

  @tailrec
  private def resolve(args: Seq[String], commands: Seq[Command[A]], errors: Seq[(Command[A], Throwable)]): Either[Seq[(Command[A], Throwable)], A] = 
    if commands.isEmpty then
      Left(errors)
    else 
      val command = commands.head
      command(args) match 
        case Success(config) =>
          Right(config)
        case Failure(error) =>
          resolve(args, commands.drop(1), errors :+ (command, error))
  