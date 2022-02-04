package urwerk.cli

import urwerk.test.TestBase

import Command.*
import Parameter.{param, toParam, trailingArgs}
import java.lang.annotation.Target
import scala.annotation.targetName

class CommandsTest extends TestBase:

  object Target: 
    def msg = "greetings"


  extension (target: Target.type)
    def echo: Unit = 
      println(s"MESG: ${target.msg}")

  Target.echo

  "" in {
    // Commands(
    //   Command := Seq(parm, param, label),
    //   Command()
    //)
  }