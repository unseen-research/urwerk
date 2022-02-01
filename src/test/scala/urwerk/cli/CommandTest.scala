package urwerk.cli

import urwerk.test.TestBase

import Command.*
import Parameter.param

class CommandTest extends TestBase:
  
  "command" in {
    val cmd = Command[Seq[String]](
      ParameterList := Seq(
        param[Int], param[Int], param[Int], param[Int]),

      ParameterList / "global" := Seq(
        param[Int], param[Int], param[Int], param[Int]),

      Description := "some command description")

  }

  
