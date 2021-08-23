package urwerk.command

object Command: 
  def apply[A](name: String): CommandParameterList[A] = new CommandParameterList{

  }

trait Command[A]:
  def usage(text: String): Command[A] = ???

  def apply(args: Seq[String]): A = ???

trait CommandParameterList[A] extends Command[A]:
  def params(param: Parameter[?, A], params: Parameter[?, A]*): CommandParameterList[A] = ???


