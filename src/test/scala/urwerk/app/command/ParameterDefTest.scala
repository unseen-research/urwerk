package urwerk.app.command

import urwerk.test.TestBase
import urwerk.source.Source

class ParameterDefTest extends TestBase:

  "params" in {
    case class Config(values: Seq[String])

    val run = Params[Config].param[Int]
      .param[Int]
      //.param[Int]
      //  .onApply((config, value) => config)
      //.cmd("run")
      //.param[String]
      //.onApply(x=> Source())
  }