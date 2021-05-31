package urwerk.maven

object Fixtures:
  val metadataXml = """
      <?xml version="1.0" encoding="UTF-8"?>
      <metadata>
        <groupId>org.foo.bar</groupId>
        <artifactId>foo-bar</artifactId>
        <versioning>
          <latest>2.0-M1</latest>
          <release>1.0</release>
          <versions>
            <version>1.0-RC-1</version>
            <version>1.0</version>
            <version>2.0-RC-1</version>
          </versions>
          <lastUpdated>20191212025101</lastUpdated>
        </versioning>
      </metadata>""".stripIndent.stripLeading()

  val versions = Set("1.0-RC-1", "1.0", "2.0-RC-1")

  val metadata = Metadata(ModuleId("org.foo.bar", "foo-bar"), versions)

