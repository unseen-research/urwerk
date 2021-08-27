package urwerk.command

import urwerk.command.Parameters.ParameterList
import urwerk.command.Parameters.Position
import scala.annotation.tailrec

object Command: 
  def apply[A](name: String): CommandParameterList[A] = new CommandParameterList[A](Seq()) with Command[A](Seq(), "")
   
trait Command[A](parameterLists: Seq[ParameterList[A]], description: String):
  def description(text: String): Command[A] = copy(description = description)

  def apply(config: A, args: Seq[String]): Option[A] = 
    collectParams(parameterLists, config, args, Position(0, 0))

  @tailrec
  private def collectParams(paramLists: Seq[ParameterList[A]], config: A, args: Seq[String], pos: Position): Option[A] =
    if paramLists.isEmpty then
      Some(config)
    else
      val paramList = paramLists.head
      val (_config, _pos) =  paramList.collectParams(config, args, pos)
      collectParams(paramLists.drop(1), _config, args, _pos)

  private[command] def copy(
      parameterLists: Seq[ParameterList[A]] = parameterLists,
      description: String = description) = 
    new Command[A](parameterLists, description){}

trait CommandParameterList[A](parameterLists: Seq[ParameterList[A]]) extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = 
    copy2(parameterLists = parameterLists :+ ParameterList(param +: params))

  private[command] def copy2(parameterLists: Seq[ParameterList[A]] = parameterLists) = 
    new CommandParameterList[A](parameterLists) with Command[A](parameterLists, "")
