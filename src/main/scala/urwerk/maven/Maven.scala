package urwerk.maven

import urwerk.io.{ByteString, Path, Uri}
import urwerk.source.Source

case class ModuleId(group: String, name: String)

case class ModuleVersionId(moduleId: ModuleId, version: Version):
  export moduleId.*

case class ModuleVersionInfo()

case class ArtifactId(moduleId: ModuleId, version: Version, extension: String, classifier: String):
  export moduleId.*

trait ArtifactSource:
  val artifactId: ArtifactId

  export artifactId.*
  
  def size: Long
  
  def uri: Uri

  def bytes: Source[ByteString]

type Version = String

object Version:
  def apply(version: String): Version = version

object Maven:
  def metadataXmlFileName = "maven-metadata.xml"

  def modulePath(moduleId: ModuleId): Path =
    Path(moduleId.group.split('.')*) / moduleId.name

  def metadataXmlPath(moduleId: ModuleId): Path =
    modulePath(moduleId) / metadataXmlFileName

  def artifactPath(artifactId: ArtifactId): Path =
    modulePath(artifactId.moduleId).resolve(artifactId.version)
      .resolve(artifactName(artifactId))
  
  def artifactName(artifactId: ArtifactId): String =
    s"${artifactId.name}-${artifactId.version}-${artifactId.classifier}.${artifactId.extension}"
  //com/typesafe/play/play_2.13/2.7.3/play_2.13-2.7.3-sources.jar
//<Name>:<Tag>
//  com/typesafe/play/play_2.13/2.7.3/play_2.13-2.7.3.jar
//  com.typesafe.play.play_2.1:2.7.3
//  com.typesafe.play.play_2.1:2.7.3&jar
//com.typesafe.play/play_2.1:2.7.3
//
//
//play_2.13-2.7.3-javadoc.jar                       2019-06-19 09:50   6029570
//com.typesafe.play.play_2.1:2.7.3&javadoc&jar
//
//play_2.13-2.7.3-playdoc.jar.asc.sha1              2019-06-19 09:50        40
//com.typesafe.play.play_2.1:2.7.3&playdoc&jar.asc.sha1
//
//play_2.13-2.7.3.pom
//com.typesafe.play.play_2.1:2.7.3&pom
//
//play_2.13-2.7.3.pom.sha1
//com.typesafe.play.play_2.1:2.7.3&playdoc&jar.asc.sha1
//
//
//play_2.13-2.7.3-javadoc.jar.asc                   2019-06-19 09:50       475
//play_2.13-2.7.3-javadoc.jar.asc.md5               2019-06-19 09:50        32
//play_2.13-2.7.3-javadoc.jar.asc.sha1              2019-06-19 09:50        40
//play_2.13-2.7.3-javadoc.jar.md5                   2019-06-19 09:50        32
//play_2.13-2.7.3-javadoc.jar.sha1                  2019-06-19 09:50        40
//play_2.13-2.7.3-playdoc.jar                       2019-06-19 09:50   2883046
//play_2.13-2.7.3-playdoc.jar.asc                   2019-06-19 09:50       475
//play_2.13-2.7.3-playdoc.jar.asc.md5               2019-06-19 09:50        32
//play_2.13-2.7.3-playdoc.jar.asc.sha1              2019-06-19 09:50        40
//play_2.13-2.7.3-playdoc.jar.md5                   2019-06-19 09:50        32
//play_2.13-2.7.3-playdoc.jar.sha1                  2019-06-19 09:50        40
//play_2.13-2.7.3-sources.jar                       2019-06-19 09:50    529882
//play_2.13-2.7.3-sources.jar.asc                   2019-06-19 09:50       475
//play_2.13-2.7.3-sources.jar.asc.md5               2019-06-19 09:50        32
//play_2.13-2.7.3-sources.jar.asc.sha1              2019-06-19 09:50        40
//play_2.13-2.7.3-sources.jar.md5                   2019-06-19 09:50        32
//play_2.13-2.7.3-sources.jar.sha1                  2019-06-19 09:50        40
//play_2.13-2.7.3.jar                               2019-06-19 09:50   2900347
//play_2.13-2.7.3.jar.asc                           2019-06-19 09:50       475
//play_2.13-2.7.3.jar.asc.md5                       2019-06-19 09:50        32
//play_2.13-2.7.3.jar.asc.sha1                      2019-06-19 09:50        40
//play_2.13-2.7.3.jar.md5                           2019-06-19 09:50        32
//play_2.13-2.7.3.jar.sha1                          2019-06-19 09:50        40
//play_2.13-2.7.3.pom                               2019-06-19 09:50      8304
//play_2.13-2.7.3.pom.asc                           2019-06-19 09:50       475
//play_2.13-2.7.3.pom.asc.md5                       2019-06-19 09:50        32
//play_2.13-2.7.3.pom.asc.sha1                      2019-06-19 09:50        40
//play_2.13-2.7.3.pom.md5                           2019-06-19 09:50        32
//play_2.13-2.7.3.pom.sha1