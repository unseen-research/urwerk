package urwerk.app

import picocli.CommandLine
import picocli.CommandLine.Command
import picocli.CommandLine
import picocli.CommandLine.Command
import picocli.CommandLine.Option
import picocli.CommandLine.Parameters
import picocli.CommandLine.ScopeType
import picocli.CommandLine.ArgGroup

import java.util.concurrent.Callable

trait ExitOp:
  def apply(status: Int): Unit

given ExitOp with
  def apply(status: Int): Unit = sys.exit(status)

@Command(name = "Tets App", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class MainCommand extends Callable[Int], Main.Application:
  @Option(names = Array("--global"), scope = ScopeType.INHERIT) // option is shared with subcommands
  var global: Int = 0

  def call(): Int = 7

given Seq[Main.Command] = Seq()

given Main.Application = MainCommand()

object Main:
  trait Application

  trait Command

  def main(args: Array[String]): Unit =
    mainOp(args)

def mainOp(using app: Main.Application, commands: Seq[Main.Command])(args: Array[String])(using exitOp: ExitOp): Unit =
  val commandLine = commands.foldLeft(CommandLine(app)){(commandLine, subcommand) =>
      commandLine.addSubcommand(subcommand)}
    .setStopAtPositional(true)

  val status = commandLine.execute(args*)
  if status != 0 then
    exitOp(status)


