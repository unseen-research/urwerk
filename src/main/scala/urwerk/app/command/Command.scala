package urwerk.app.command

import urwerk.app.command.Parameters.ParameterList
import urwerk.app.command.Parameters.Position
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Command: 
  def apply[A](name: String): CommandParameterList[A] = new CommandParameterList[A](Seq()) with Command[A](Seq(), "")
   
trait Command[A](parameterLists: Seq[ParameterList[A]], description: String):
  def description(text: String): Command[A] = copy(description = description)

  def apply(config: A, args: Seq[String]): Try[A] = 
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
      parameterLists: Seq[ParameterList[A]] = parameterLists,
      description: String = description) = 
    new Command[A](parameterLists, description){}

trait CommandParameterList[A](parameterLists: Seq[ParameterList[A]]) extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = 
    copy2(parameterLists = parameterLists :+ ParameterList(param +: params))

  private[command] def copy2(parameterLists: Seq[ParameterList[A]] = parameterLists) = 
    new CommandParameterList[A](parameterLists) with Command[A](parameterLists, "")
