package urwerk.command

import urwerk.test.TestBase
import urwerk.command.Parameters.ParameterException

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "command match" in {
    val cmd = Command("command")
      .params(
        param[String]("name1"){(value, config) =>
          config :+ s"name1-$value"
        },
        param[Int]("name2"){(value, config) =>
          config :+ s"name2-$value"
        })
      .params(
        param[String]((value, config) =>
            config :+ s"cmd-$value")
          .accept(_ == "command"))
      .params(
        param[String]{(value, config) =>
            config :+ s"param-$value"}
          .arity(0, 77)
          .accept(_ => true))

    val config = cmd(Seq(), Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
    config should be (Seq("name2-55", "name1-value1", "cmd-command", "param---name", "param-value"))
  }

  "command not match" in{
    val cmd = Command("help")
      .params(
        param[Unit]("help", "h")((value, config) =>
            config :+ s"help-$value")
          .arity(1, 10))

    val ex = intercept[ParameterException]{
      cmd(Seq(), Seq("--version"))
    } 
    
    println(ex)
  }


