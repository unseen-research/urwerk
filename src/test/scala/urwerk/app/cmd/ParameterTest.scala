package urwerk.app.cmd

import urwerk.test.TestBase

class ParameterTest extends TestBase:

  "default value" in {
    val param = Parameter[String](None).default("some value")

    param.default.get should be("some value")
  }