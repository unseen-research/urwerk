package urwerk.command

import urwerk.test.TestBase

class ParametersTest extends TestBase:
  "create paramerters" in {
    case class Config(value: Int)

    val params = Parameters(Config(0))
    import params.*

    val value = param[String]
      .collect((value, config) => config.copy(value = value.toInt))
      .doCollect("77")

    value should be (Config(77))  
  }
