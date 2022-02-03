package urwerk.cli

import urwerk.test.TestBase

import Command.*
import Parameter.param

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
        param[Int]((v, c) => c :+ v), 
        param[Int]((v, c) => c :+ v)),
      ParameterList := Seq(
        param[Int]((v, c) => c :+ v), 
        param[Int]((v, c) => c :+ v)),
      Action := {config =>
        config.sum
      })

    cmd.execute("1", "2", "3", "4") should be (10)
  }
  
