package urwerk.app.cmd

import urwerk.test.TestBase

class ParameterTest extends TestBase:

  import Parameter.*

  "default value" in {
    val param = Parameter[String]().default("some value")

    param.default.get should be("some value")
  }

  "on apply" in {
    case class Config(value: String)
    given ConfigProvider[Config] = () => Config("")

    Parameter[String]().onApply((config: Config, value) => config.copy(value = "test" + value)).apply("any value")
  }