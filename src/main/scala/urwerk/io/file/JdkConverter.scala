package urwerk.io.file

import java.io.File as JFile
import java.nio.file.Path as JPath

import urwerk.io.Path

extension (path: Path)
  def toFile: JFile =
    val root = if path.absolute then "/" else ""
    val head = root + path.elements.applyOrElse(0, _ => "")
    val tail = path.elements.drop(1)
    JPath.of(head, tail*).toFile