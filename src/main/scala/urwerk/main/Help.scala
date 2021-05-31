package urwerk.main

import urwerk.cli.*
import urwerk.source.Source

val helpOption = option[Unit]("help", "h")
  .map(_ => true)

val help =
  """
  Usage: urwerk [OPTIONS] COMMAND
  """.stripIndent

private def isHelp(options: Map[String, Any]): Boolean =
  options.get(helpOption.name)
    .map(_.asInstanceOf[Boolean])
    .getOrElse(false)
