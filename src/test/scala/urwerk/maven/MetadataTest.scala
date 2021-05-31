package urwerk.maven

import urwerk.test.TestBase

class MetadataTest extends TestBase:
  "versions from xml string" in {
    val metadata = Metadata.fromXml(Fixtures.metadataXml)

    metadata.versions should be (Fixtures.versions)
  }

  "group id and artifact id from xml string" in {
    val metadata = Metadata.fromXml(Fixtures.metadataXml)

    metadata.group should be ("org.foo.bar")
    metadata.name should be ("foo-bar")
  }
