package urwerk.command

import urwerk.test.TestBase

class CommandTest extends TestBase:

  "test" in {
    case class Config()

    val params = Parameters[Config]
    import params.*

    // val runCmd = Command[Config].params(
    //     param[String],
    //     param[String]
    //   )
    //   .params(
    //     param["run"]
    //   )
    //   .usage("run command")
  }