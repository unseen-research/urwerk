package urwerk.maven

import scala.xml.{Node, XML}

object Metadata {

  val FileName: String = "maven-metadata.xml"

  def fromXml(metadata: String): Metadata = {
    val topNode = XML.loadString(metadata)
    Metadata(
      ModuleId(
        extractGroupId(topNode),
        extractArtifactId(topNode)),
      extractVersions(topNode))
  }

  private def extractVersions(top: Node): Set[Version] = {
    val versions = for{
      version <- top \ "versioning" \ "versions" \ "version"}
    yield{
      version.text}
    versions.toSet
      .map(Version(_))
  }

  private def extractGroupId(top: Node): String = {
    val values = for{
      node <- top \ "groupId"}
    yield{
      node.text}
    values.headOption.getOrElse("")
  }

  private def extractArtifactId(top: Node): String = {
    val values = for{
      node <- top \ "artifactId"}
    yield{
      node.text}
    values.headOption.getOrElse("")
  }
}

case class Metadata(moduleId: ModuleId, versions: Set[Version]):
  export moduleId.*
