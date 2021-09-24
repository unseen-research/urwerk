package urwerk.io.file

import java.nio.file.Paths

import scala.jdk.CollectionConverters.given

import urwerk.io

type Path = java.nio.file.Path

object Path:
  def apply(element: String, elements: String*): Path =
    Paths.get(element, elements*)

  def apply(path: io.Path): Path =
    apply(path.toString)

val Cwd: Path = Path("").toAbsolutePath

trait PathOps:
  extension (path: Path)
    infix def /(element: String): Path =
      path.resolve(element)

    def toSeq: Seq[String] =
      val elems = path.iterator.asScala.toSeq.map(_.toString)
      if path.isAbsolute then "/" +: elems
      else elems

given PathOps = new PathOps{}
