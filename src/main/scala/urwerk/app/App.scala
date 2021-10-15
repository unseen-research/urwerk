package urwerk.app

import scala.concurrent.Future
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.CommandCreation

case class App(commands: Seq[CommandCreation]):
  final def main(args: Array[String]): Unit =
    System.out.println("Greetings")
    System.err.println("Mortal")
