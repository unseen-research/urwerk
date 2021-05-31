package urwerk.maven

import urwerk.io.Path
import urwerk.test.TestBase

import java.net.http.HttpResponse
import scala.concurrent.Future

class MavenTest extends TestBase:
  "version" in {
    val version = Version("1.0")
    version should be ("1.0")
  }

  "module path" in {
    val path = Maven.modulePath(ModuleId("org.foo.bar", "foo-bar"))

    path should be(Path("org", "foo", "bar", "foo-bar"))
  }

  "metadata xml path" in {
    val path = Maven.metadataXmlPath(ModuleId("org.foo.bar", "foo-bar"))

    path should be(Path("org", "foo", "bar", "foo-bar", "maven-metadata.xml"))
  }

  "artifact name" in {
    val name = Maven.artifactName(ArtifactId(ModuleId("org.foo.bar", "foo-bar"), "1.0.1", "jar", "sources"))
    name should be ("foo-bar-1.0.1-sources.jar")
  }

  "artifact path" in {
    val path = Maven.artifactPath(ArtifactId(ModuleId("org.foo.bar", "foo-bar"), "1.0.1", "jar", "sources"))
    path should be (Path("org", "foo", "bar", "foo-bar", "1.0.1", "foo-bar-1.0.1-sources.jar"))
  }