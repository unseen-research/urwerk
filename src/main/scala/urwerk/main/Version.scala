package urwerk.main

import urwerk.cli.*
import urwerk.source.Source

val versionOption = option[Unit]("version", "v")
  .map(_ => true)

def version: String = "1.0"

private def versionRequested(options: Map[String, Any]): Boolean =
  options.get(versionOption.name)
    .map(_.asInstanceOf[Boolean])
    .getOrElse(false)