package urwerk.command

import urwerk.test.TestBase

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "apply command" in {
    val cmd = Command("command")
      .params(
        param[String]("name1"){(value, config) =>
          config :+ s"name1-$value"
        },
        param[Int]("name2"){(value, config) =>
          config :+ s"name2-$value"
        })
      .params(
        param[String].accept(_ == "command"){(value, config) =>
          config :+ s"cmd-$value"
        })
      .params(
        param[String]{(value, config) =>
            config :+ s"param-$value"}
          .arity(0, 77)
          .accept(_ => true))

    val Some(config) = cmd(Seq(), Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
    config should be (Seq("name2-55", "name1-value1", "cmd-command", "param---name", "param-value"))
  }

