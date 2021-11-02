package urwerk.maven

import java.nio.file.{Files, NoSuchFileException}
import java.nio.file.attribute.BasicFileAttributes

import urwerk.io.{ByteString, Path, Uri}
import urwerk.io.file.given
import urwerk.io.file
import urwerk.source.{Optional, Singleton, Source}

class FileSystemRepository
//class FileSystemRepository(val path: file.Path) extends Repository:


  // def uri: Uri = path.toUri

  // def listVersions(moduleId: ModuleId): Source[Version] =
  //   path.resolve(
  //       file.Path(Maven.modulePath(moduleId)))
  //     .directories
  //     .map(_.elements.last)

  // def metadataXml(moduleId: ModuleId): Source[String] =
  //   path.resolve(
  //     Maven.metadataXmlPath(moduleId))
  //     .strings

  //   Source(moduleId)
  //     .map(modId =>
  //       path.resolve(Maven.metadataXmlPath(modId)))
  //     .flatMap(path =>
  //       if Files.isRegularFile(file.Path(path)) then path.strings
  //       else Source("")
  //       //        else if Files.isDirectory(path) then Source.error(NoSuchFileException())
  //       //        else if !Files.isDirectory(path.parent)
  //     )

  // def metadata(moduleId: ModuleId): Optional[Metadata] =
  //   metadataXml(moduleId)
  //     .mkString
  //     .filter(_.nonEmpty)
  //     .map(Metadata.fromXml(_))

  // def artifact(id: ArtifactId): Source[ArtifactSource] =
  //   val artifactPath = Maven.artifactPath(id)
  //   Source(path.resolve(artifactPath)).zipWithAttributes[BasicFileAttributes]
  //     .map{(path, attribs) =>
  //       val _size: Long = attribs.size
  //       new ArtifactSource:
  //         val artifactId: ArtifactId = id
  //         def size: Long = _size
  //         def uri: Uri = Path(path).toUri
  //         def bytes: Source[ByteString] = path.bytes
  //     }
