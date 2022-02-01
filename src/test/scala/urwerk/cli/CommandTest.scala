package urwerk.cli

import urwerk.test.TestBase

import Command.*
import Parameter.param

class CommandTest extends TestBase:
  
  "command" in {
    val cmd = Command[Seq[String]](
      paramList("global") := Seq(
        param[Int], param[Int], param[Int], param[Int]),
      description := "some command description")

  }

  
