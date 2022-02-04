package urwerk.cli

import urwerk.test.TestBase

import Command.*
import Parameter.{param, toParam, trailingArgs}

class CommandTest extends TestBase:
  
  "command setting" - {

    val cmd = Command(Seq[String]())(
      ParameterList := Seq(
        param[Int]),

      ParameterList / "global" := Seq(
        param[Int]),

      Description := "some command description")

    "description" in {
      cmd.description should be ("some command description")
    }

    "parameter lists" in {
      val paramLists = cmd.parameterLists
      paramLists(0).label should be ("")
      paramLists(1).label should be ("global")
    }
  }

  "execute action with single parameter list" in {
    val cmd = Command(Seq[Int]())(
      ParameterList := Seq(
        param[Int]((config, value) => config :+ value), 
        param[Int]((config, value) => config :+ value)),
      ParameterList := Seq(
        param[Int]((config, value) => config :+ value), 
        param[Int]((config, value) => config :+ value)),
      Action := {config =>
        config.sum
      })

    cmd.execute("1", "2", "3", "4") should be (10)
  }
  
  "unknown positional parameter" in {
    val cmd = Command(Seq[Int]())(
      ParameterList := Seq(
        param[Int]((config, value) => config :+ value)),
      ParameterList := Seq( 
        param[Int]((config, value) => config :+ value)))

    val exception = intercept[UnknownParameterException]{
      cmd.execute("1", "2", "3")}

    exception.position should be (Position(2, 0))
  }

  "unknown named parameter" in {
    val cmd = Command(Seq[Int]())(
      ParameterList := Seq(
        param[Int]((config, value) => config :+ value)),
      ParameterList := Seq( 
        param[Int]((config, value) => config :+ value)))

    val exception = intercept[UnknownParameterException]{
      cmd.execute("1", "--name", "3")}

    exception.position should be (Position(1, 0))
  }

  "subcommand with context parameters and trailing parameters" in {
    val cmd = Command(Seq[Int]())(
      ParameterList / "context" := Seq(
        param[Int]("param1")((config, value) => 
          config :+ value),
        param[Int]("param2")((config, value) => 
          config :+ value)),
      ParameterList / "command" := Seq(
        "run".toParam.required,
        param[Int]((config, value) => 
            config :+ value)
          .required),
      ParameterList / "command args" := Seq(
        trailingArgs.apply((config, value) => config :+ value.toInt)),
      Action := {config =>
        config.sum
      }
    )

    val status = cmd.execute("--param1", "1", "--param2", "2", "run", "3", "4", "5", "6")

    status should be (21)
  }