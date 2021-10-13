package urwerk.app

import scala.concurrent.Future
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.Command

case class App(commands: Seq[Command])

trait Main(commands: Source[Optional[(Source[String], Source[String])]]):

  def main(args: Array[String]): Unit =
    println(s"Args: ${args.mkString}")

    commands.concat.flatMap((std, err)=>
        std.merge(err))
      .doOnNext(println(_))
      .doOnError(println(_))
      .lastOption.block




