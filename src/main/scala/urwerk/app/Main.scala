package urwerk.app

import picocli.CommandLine

object Main extends Main(""):
  println()

class Main(mainCommand: AnyRef, subcommands: AnyRef*):
  def main(args: Array[String]): Unit =
    val commandLine = subcommands.foldLeft(CommandLine(mainCommand)){(commandLine, subcommand) =>
        commandLine.addSubcommand(subcommand)}
      .setStopAtPositional(true)

    val status = commandLine.execute(args*)
    if status != 0 then
      exit(status)

  def exit(status: Int): Unit = sys.exit(status)