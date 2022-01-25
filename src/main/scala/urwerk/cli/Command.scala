package urwerk.cli

trait Command[C]():
  def parameterList(): Command[C] = ???

  def execute(op: C => Int): Int = ???


object Command: 
  import urwerk.cli.Command as CommandIf

  case class Command[C]() extends CommandIf[C]

