package urwerk.app.command

import urwerk.test.TestBase
import urwerk.source.Source

class ParameterDefTest extends TestBase:

  "params" in {
    case class Configx(values: Seq[String])

    val run = Params[Configx]
      .param[Int]
      .param[String]
      //  .onApply((config, value) => config)
      .cmd("run")
      //.param[String]
      .onApply(x=> Source())
  }