package urwerk.app

import picocli.CommandLine

trait ExitOp:
  def apply(status: Int): Unit

given ExitOp with
  def apply(status: Int): Unit = sys.exit(status)

object Main:
  trait Application

  trait Command

  def main(args: Array[String]): Unit =
    mainOp(args)

def mainOp(args: Array[String])(using app: Main.Application, commands: Seq[Main.Command], exitOp: ExitOp): Unit =
  val commandLine = commands.foldLeft(CommandLine(app)){(commandLine, subcommand) =>
      commandLine.addSubcommand(subcommand)}
    .setStopAtPositional(true)

  val status = commandLine.execute(args*)
  if status != 0 then
    exitOp(status)


