package urwerk.main

import urwerk.cli.*
import urwerk.source.Source
import urwerk.source.Optional

val helpOption = option[Boolean]("help", "h")
  .arity(1, 1)
  .map(_ => true)

val help =
  """
  Usage: urwerk [OPTIONS] COMMAND
  """.stripIndent

object Help:
  def apply(args: Seq[String]): Optional[Source[Either[String, String]]] = 
    try
      val (opts, tail) = args.extractOptions(option[Unit]("help", "h")) 
      if tail.isEmpty then
        Optional(Source(Right(help)))
      else 
        Optional.error(IllegalArgumentException())
    catch
      case _: NoSuchOptionException =>
        Optional.empty
      case e: Throwable =>
        Optional.error(e)

        

    

