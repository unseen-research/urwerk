package urwerk.app.command

import urwerk.app.command.Parameters.ParameterList
import urwerk.app.command.Parameters.Position
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import urwerk.source.Source
import urwerk.source.Optional

object Command: 
  def apply[A](name: String, config: A): CommandParameterList[A] = InnerCommandParameterList[A](InnerCommand[A](name, config, Seq(), _ => {Optional.empty}, ""))
   
  private class InnerCommandParameterList[A](val innerCmd: InnerCommand[A]) extends CommandParameterList[A]:
    def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = 
      InnerCommandParameterList[A](
        innerCmd.copy(parameterLists = innerCmd.parameterLists :+ ParameterList(param +: params))
      )

    export innerCmd.*

  private[command] class InnerCommand[A](val name: String,
      config: A, 
      val parameterLists: Seq[ParameterList[A]], 
      val applyOp: A => Optional[(Source[String], Source[String])],       
      description: String) extends Command[A]:
    def description(text: String): Command[A] = copy(description = description)

    def apply(op: A => Optional[(Source[String], Source[String])]): Command[A] = 
      copy(applyOp = op)
      
    def resolve(args: Seq[String]): Optional[(Source[String], Source[String])] = 
      collectParams(parameterLists, config, args, Position(0, 0)) match
        case Success(config) =>
          applyOp(config)
        case Failure(error: Throwable) => 
          Optional.error(error)

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
        applyOp: A => Optional[(Source[String], Source[String])] = applyOp,
        description: String = description) = 
      InnerCommand[A](name, config, parameterLists, applyOp, description)

trait Command[A]:  
  def name: String
  def description(text: String): Command[A]
  def apply(op: A => Optional[(Source[String], Source[String])]): Command[A]
  def resolve(args: Seq[String]): Optional[(Source[String], Source[String])]

trait CommandParameterList[A] extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A]

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

  //@tailrec
  private def resolve(args: Seq[String], commands: Seq[Command[A]], errors: Seq[(Command[A], Throwable)]): Either[Seq[(Command[A], Throwable)], A] = 
    if commands.isEmpty then
      Left(errors)
    else 
      ???
      // val command = commands.head
      // command(args) match 
      //   case Success(config) =>
      //     Right(config)
      //   case Failure(error) =>
      //     resolve(args, commands.drop(1), errors :+ (command, error))
  