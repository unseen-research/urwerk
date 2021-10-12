package urwerk.app.command

import scala.util.Success
import scala.util.Failure

import urwerk.test.TestBase
import urwerk.app.command.Parameters.ParameterException
import urwerk.app.command.Parameters.MissingParameterException
import urwerk.app.command.Parameters.Position
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.Parameters.UnexpectedParameterException

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  // "materialize" in {
  //   val src = Source(
  //     Optional(),
  //     Optional.error(IllegalArgumentException("dkfj")),
  //     Optional(Source("abc", "def")),
  //     Optional(Source("xyz", "ijk")))

  //   src
  //     .flatMap(src => src.materialize)
  //     //.materialize
  //     .doOnNext(n => println(s"next $n"))

  //     .toSeq.block

  // }

  "command with multiple param lists" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1"){(value, config) =>
          config :+ s"name1:$value"
        },
        param[Int]("name2"){(value, config) =>
          config :+ s"name2:$value"
        })
      .params(
        param[String]((value, config) =>
            config :+ s"cmd:$value")
          .accept(_ == "command"))
      .params(
        param[Int]("name3"){(value, config) =>
          config :+ s"name3:$value"
        })
      .apply{config =>
        Optional((Source(config*), Source()))}

    val result = cmd.withArgs(Seq("--name2", "55", "--name1", "value1", "command", "--name3", "77"))
      .flatMap((stdOut, errOut) =>  Source(stdOut, errOut))
      .concat.toSeq.block

    result should be (Seq("name2:55", "name1:value1", "cmd:command", "name3:77"))
  }

  "command with multiple param lists with undefined trailing args" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1"))
      .params(
        param[Int]("name2"))
    val ex = intercept[UnexpectedParameterException]{
      cmd.withArgs(Seq("--name1", "value1", "--name2", "77", "undefined1", "undefined2"))
        .block
    }
    ex.position should be (Position(4, 0))
  }

  "command with multiple param lists with undefined trailing flags" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1"))
      .params(
        param[Int]("name2"),
        param[Unit]("a"),
        param[Unit]("b"),
        param[Unit]("c"))
    val ex = intercept[UnexpectedParameterException]{
      cmd.withArgs(Seq("--name1", "value1", "--name2", "77", "-abcUndefined", ""))
        .block
    }
    ex.position should be (Position(4, 3))
  }

  "command with multiple param lists and trailing args" in {
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
        Optional((Source(config*), Source()))}

    val result = cmd.withArgs(Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
      .flatMap((stdOut, errOut) =>  Source(stdOut, errOut))
      .concat.toSeq.block

    result should be (Seq("name2-55", "name1-value1", "cmd-command", "param---name", "param-value"))
  }

  "command does not match" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("param1"){(value, config) =>
            config :+ s"param1-$value"}
          .default("default-value")
      )
      .params(
        param[Int]{(value, config) =>
            config :+ s"int-param-$value"}
          .arity(1, 1)
      )
      .apply{config =>
        Optional((Source(config*), Source()))}

    val result = cmd.withArgs(Seq("value1", "value2"))
       .toSeq.block

    result should be (Seq())
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