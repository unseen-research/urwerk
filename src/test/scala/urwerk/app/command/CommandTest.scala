package urwerk.app.command

import scala.util.Success
import scala.util.Failure

import urwerk.test.TestBase
import urwerk.app.command.Parameters.ParameterException
import urwerk.app.command.Parameters.MissingParameterException
import urwerk.app.command.Parameters.Position
import urwerk.source.Source
import urwerk.source.Optional

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "command" - {
    "match" in {
      val cmd = Command("command", Seq[String]())
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
        .apply{config =>
          Optional((Source(config*), Source()))
        }


      // val src = cmd.resolve(Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
      // //val Success(config) = cmd(Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
      // //config should be (Seq("name2-55", "name1-value1", "cmd-command", "param---name", "param-value"))
    }

  //   "not match" in{
  //     val cmd = Command("help", Seq[String]())
  //       .params(
  //         param[Unit]("help", "h")((value, config) =>
  //             config :+ s"help-$value")
  //           .arity(1, 10))

  //     val Failure(ex: MissingParameterException) = cmd(Seq("--version"))
      
  //     ex.labelOrName should be ("help")
  //     ex.requiredArity should be (1)
  //     ex.repetition should be (0)
  //     ex.position should be (Some(Position(0, 0)))
  //   }
  // }

  // "commands" - {
  //   trait Callable:
  //     def apply(): String
    
  //   case class Config(value: String) extends Callable:
  //     def apply() = value
          
  //   val params = Parameters[Config]
  //   import params.*

  //   val cmds = Commands(
  //     Command("version", Config(""))
  //       .params(
  //         param[Unit]("version", "v")
  //           .arity(1, 1)),
  //     Command("run", Config(""))
  //       .params(
  //         param[String]{(value, config) =>
  //             config.copy(value + " invoked")}
  //           .accept(_ == "run")
  //           .arity(1, 1)
  //         ))

  //   "resolve command" in {
  //     val result = cmds.resolve(Seq("run")).apply()
  //     result should be ("run invoked")
  //   }

  //   "resolve failure" in {
  //     val result = cmds.onError{errors => 
  //         Config(
  //           errors.map((cmd, error) => cmd.name + ":" + error.getClass().getSimpleName()).mkString("", "&", ""))
  //       }
  //       .resolve(Seq("undefined")).apply()
      
  //     result should be ("version:MissingParameterException&run:MissingParameterException")
  //   }
  // }