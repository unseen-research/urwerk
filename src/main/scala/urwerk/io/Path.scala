package urwerk.io

import java.util.NoSuchElementException
import Path.*
import scala.collection.immutable.ArraySeq

case class Path private (val absolute: Boolean, val elements: Seq[String]):
  infix def /(element: String): Path =
    resolve(element)

  def parent: Path = parentOption.get

  def parentOption: Option[Path] =
    if elements.size <= 0  then
      None
    else if elements.size == 1 && !absolute then
      None
    else
      Some(Path(absolute, elements.dropRight(1)))

  def resolve(element: String): Path =
    Path(absolute, elements ++ resolveElement(element))

  def resolve(path: Path): Path =
    if path.absolute then path
    else Path(absolute, elements ++ path.elements)

  override def toString: String =
    val start = if absolute then "/" else ""
    elements.mkString(start, "/", "")

  export elements.*

object Path:
//  given Conversion[Path, JNFPath] with {
//    def apply(path: Path): JNFPath = {
//      val root = if path.absolute then "/" else ""
//      val head = root + path.elements.applyOrElse(0, _ => "")
//      val tail = path.elements.drop(1)
//      JNFPath.of(head, tail*)
//    }
//  }
//
//  given Conversion[JNFPath, Path] with {
//    def apply(path: JNFPath): Path = {
//      Path(path.toString)
//    }
//  }

  def apply(elements: String*) =
    new Path(isAbsolute(elements), resolveElements(elements))

  def from(elements: Seq[String]): Path =
    apply(elements*)

  inline private def isAbsolute(elems: Seq[String]): Boolean =
    elems.headOption.map(_.startsWith("/")).getOrElse(false)

  private def resolveElements(elements: Seq[String]): Seq[String] =
    elements.flatMap(resolveElement(_))

  private def resolveElement(elem: String): Seq[String] =
    ArraySeq.unsafeWrapArray(elem.split('/')).filter(_.nonEmpty)
