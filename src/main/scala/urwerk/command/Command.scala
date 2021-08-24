package urwerk.command

import urwerk.command.Parameters.ParameterList

object Command: 
  def apply[A](name: String): CommandParameterList[A] = new CommandParameterList[A](Seq()) with Command[A](Seq(), "")
   
trait Command[A](parameterLists: Seq[ParameterList[A]], description: String):
  def description(text: String): Command[A] = copy(description = description)

  def apply(config: A, args: Seq[String]): A = 
    case class Config[A](config: A, args: Seq[String])

    val _config = parameterLists.foldLeft(Config(config, args)){case (Config(config, args), paramList) =>
      val (_config, remainingArgs) =  paramList.collectParams(config, args)
      Config(_config, remainingArgs)
    }

    _config.config

  private[command] def copy(
      parameterLists: Seq[ParameterList[A]] = parameterLists,
      description: String = description) = 
    new Command[A](parameterLists, description){}

trait CommandParameterList[A](parameterLists: Seq[ParameterList[A]]) extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = 
    copy2(parameterLists = parameterLists :+ ParameterList(param +: params))

  private[command] def copy2(parameterLists: Seq[ParameterList[A]] = parameterLists) = 
    new CommandParameterList[A](parameterLists) with Command[A](parameterLists, "")
