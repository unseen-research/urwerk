package urwerk.cli

import urwerk.test.TestBase
import Cmd.*

class CmdTest extends TestBase:
  
  "value setting" in {
    case class Config()

    val cmd = Cmd[Config](
      Value / "any-name" := "any-value", 
      Value / "other-name" := "other-value")

    cmd.settings should be(Seq(ValueBinding("any-name", "any-value"), ValueBinding("other-name", "other-value")))
  }

  "action setting" in {
    case class Config(value: Int)

    val cmd = Cmd[Config](Action := {config => config.value + 1})

    cmd.action(Config(77)) should be (78)
  }

  "execute with values" in {
    case class Config(a: String, b: Int, c: Boolean)

    val cmd = Cmd[Config](
      Value / "a" := "any-value", 
      Value / "b" := 7,
      Value / "c" := true,
      Action := {config => config}
      )

    cmd.execute() should be(Config("any-value", 7, true))
  }
