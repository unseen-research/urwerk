package urwerk.maven

import urwerk.io.http.Header.ContentLength
import urwerk.io.http.Request.Get
import urwerk.io.http.Response
import urwerk.io.http.client.Http
import urwerk.io.{ByteString, Path, Uri}
import urwerk.io.Uri.path
import urwerk.source.{Optional, Source}

import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

class HttpRepository(val uri: Uri) extends Repository:

  def listVersions(moduleId: ModuleId): Source[Version] =
    metadata(moduleId)
      .flatMap(metadata => 
        Source.from(metadata.versions))

  def metadataXml(moduleId: ModuleId): Source[String] =
    val metadataPath = Maven.metadataXmlPath(moduleId)
    val absoluteMetadataPath = uri.path.resolve(metadataPath)
    val metadataUri = uri.resolve(absoluteMetadataPath.toString)

    Http(Get(metadataUri))
      .flatMap(response =>
        response.body.map(_.toString)
      )

  def metadata(moduleId: ModuleId): Optional[Metadata] =
    metadataXml(moduleId)
    .mkString
    .filter(_.nonEmpty)
    .map(Metadata.fromXml(_))

  def artifact(id: ArtifactId): Source[ArtifactSource] =
    val artifactPath = Maven.artifactPath(id)
    val absolutePath = uri.path.resolve(artifactPath)
    val artifactUri = uri.resolve(absolutePath.toString)

    Http(Get(artifactUri))
      .map{response =>
        val length = response.headers[ContentLength]

        new ArtifactSource:
          val artifactId = id
          def size: Long = length.getOrElse(-1L)
          def uri: Uri = artifactUri
          def bytes: Source[ByteString] = response.body
      }