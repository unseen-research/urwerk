package urwerk.command

object Command: 
  def apply[A]: CommandParamDef[A] = new CommandParamDef{}

trait Command[A]:
  def usage(text: String): Command[A] = ???

trait CommandParamDef[A] extends Command[A]:
  
  def params(param: Parameter[? <: AnyRef , A], params: Parameter[? <: AnyRef , A]*): CommandParamDef[A] = ???


