package urwerk.maven

import urwerk.io.file.ReadOptions
import urwerk.io.{ByteString, Path}
import urwerk.io.file.given
import urwerk.source.TestOps.sourceProbe
import urwerk.test.{TestBase, uniqueDirectory, uniqueFile, uniquePath}

import java.nio.file.{Path => JNFPath}
import java.nio.file.Files

import scala.util.Random
import scala.language.implicitConversions

class FileSystemRepositoryTest extends TestBase:

  "metadata xml" in {
    val repo = repoWithFile(
      Path("org", "foo", "bar", "foo-bar", "maven-metadata.xml"), Fixtures.metadataXml)

    val metadataXml = repo
      .metadataXml(ModuleId("org.foo.bar", "foo-bar"))
      .mkString.block

    metadataXml should be (Fixtures.metadataXml)
  }

  "metadata xml empty if file not exists" in {
    val repo = repository
    val modulePath = repo.path / "org" / "foo" / "bar" / "foo-bar"

    Files.createDirectories(modulePath)

    val metadataXml = repo
      .metadataXml(ModuleId("org.foo.bar", "foo-bar"))
      .mkString.block

    metadataXml should be ("")
  }

  "metadata" in {
    val repo = repoWithFile(
      Path("org", "foo", "bar", "foo-bar", "maven-metadata.xml"), Fixtures.metadataXml)

    val metadata = repo
      .metadata(ModuleId("org.foo.bar", "foo-bar"))
      .block.get

    metadata.moduleId should be (Fixtures.metadata.moduleId)
    metadata.versions should be (Fixtures.metadata.versions)
  }

  "list versions" in {
    val repo = repository
    val modulePath = repo.path.resolve(Path("org", "foo", "bar", "foo-bar"))
    Files.createDirectories(modulePath / "1")
    Files.createDirectories(modulePath / "2.1")
    Files.createDirectories(modulePath / "3.2.1")

    val versions = repo
      .listVersions(ModuleId("org.foo.bar", "foo-bar"))
      .toSeq.block.toSet

    versions should be (Set("1", "2.1", "3.2.1"))
  }

  "artifact" in {
    val artifactId = ArtifactId(ModuleId("org.foo.bar", "foo-bar"), "1.0.1", "jar", "sources")
    val givenBytes = Random.nextBytes(10)
    val repo = repoWithFile(Path("org", "foo", "bar", "foo-bar", "1.0.1", "foo-bar-1.0.1-sources.jar"), givenBytes)
    val actual = repo.artifact(artifactId)
      .map{artifactSrc=>
        (artifactSrc.artifactId, artifactSrc.uri, artifactSrc.size, artifactSrc.bytes.head.block)
      }
      .head.block

    actual should be((artifactId, repo.path.resolve(Maven.artifactPath(artifactId)).toUri, 10, ByteString(givenBytes)))
  }

  def repoWithFile(file: Path, content: String): FileSystemRepository =
    repoWithFile(file,  content.getBytes)

  def repoWithFile(file: Path, content: Array[Byte]): FileSystemRepository =
    val repo = repository
    val repoFile = repo.path.resolve(file)
    Files.createDirectories(repoFile.parent)
    Files.write(repoFile, content)
    repo

  def repository: FileSystemRepository =
    val repoPath = uniqueDirectory
    repoPath.absolute should be (false)
    FileSystemRepository(repoPath)

