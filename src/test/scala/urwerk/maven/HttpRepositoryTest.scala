package urwerk.maven

import com.github.tomakehurst.wiremock.client.WireMock.*
import urwerk.io.{ByteString, Uri}
import urwerk.io.http.WiremockServer
import urwerk.test.TestBase

import scala.util.Random

class HttpRepositoryTest extends TestBase with WiremockServer:

  "metadata xml" in {
    httpServer.stubFor(get(s"/maven/2/org/foo/bar/foo-bar/maven-metadata.xml")
      .willReturn(aResponse()
        .withBody(Fixtures.metadataXml)))

    val metadataXml = HttpRepository(Uri(s"${serverUrl}/maven/2/"))
      .metadataXml(ModuleId("org.foo.bar", "foo-bar"))
      .mkString.block

    metadataXml should be (Fixtures.metadataXml)
  }

  "metadata" in {
    httpServer.stubFor(get(s"/maven/2/org/foo/bar/foo-bar/maven-metadata.xml")
      .willReturn(aResponse()
        .withBody(Fixtures.metadataXml)))

    val metadata = HttpRepository(Uri(s"${serverUrl}/maven/2/"))
      .metadata(ModuleId("org.foo.bar", "foo-bar"))
      .block.get

    metadata.moduleId should be (Fixtures.metadata.moduleId)
    metadata.versions should be (Fixtures.metadata.versions)
  }

  "versions" in {
    httpServer.stubFor(get(s"/maven/2/org/foo/bar/foo-bar/maven-metadata.xml")
      .willReturn(aResponse()
        .withBody(Fixtures.metadataXml)))

    val versions = HttpRepository(Uri(s"${serverUrl}/maven/2/"))
      .listVersions(ModuleId("org.foo.bar", "foo-bar"))
      .toSeq.block.toSet

    versions should be (Fixtures.metadata.versions)
  }

  "artifact" in {
    val artifactId = ArtifactId(ModuleId("org.foo.bar", "foo-bar"), "1.0.1", "jar", "sources")
    val givenBytes = Random.nextBytes(10)

    httpServer.stubFor(get(s"/maven/2/org/foo/bar/foo-bar/1.0.1/foo-bar-1.0.1-sources.jar")
      .willReturn(aResponse()
        .withHeader("content-length", "10")
        .withBody(givenBytes)))

    val actual = HttpRepository(Uri(s"${serverUrl}/maven/2/")).artifact(artifactId)
      .map{artifactSrc =>
        (artifactSrc.artifactId, artifactSrc.uri, artifactSrc.size, artifactSrc.bytes.head.block)
      }
      .head.block

    actual should be((artifactId, Uri(s"${serverUrl}/maven/2/org/foo/bar/foo-bar/1.0.1/foo-bar-1.0.1-sources.jar"), 10, ByteString(givenBytes)))
  }