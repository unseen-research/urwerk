package urwerk.maven

import urwerk.io.{ByteString, Uri}
import urwerk.source.{Optional, Singleton, Source}

trait Repository:
  def uri: Uri

  def listVersions(moduleId: ModuleId): Source[Version]

  def metadataXml(moduleId: ModuleId): Source[String]

  def metadata(moduleId: ModuleId): Optional[Metadata]

  def artifact(artifactId: ArtifactId): Source[ArtifactSource]

//  def moduleVersionInfo(moduleVersionId: ModuleVersionId): Singleton[ModuleVersionInfo]
//
//  def artifactBytes(artifactId: ArtifactId): Source[ByteString]
//
//  def artifactStrings(artifactId: ArtifactId): Source[String]
//
