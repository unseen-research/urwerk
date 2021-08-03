package urwerk.cli

import urwerk.test.TestBase

class CommandTest extends TestBase:
  
  case class Opt(name: String)  

  object Cmd:
    def apply[A](opts: Opt*) = "???"
    def apply[A](opts: Opt*)(id: String)(opts2: Opt*) = "???"

  "overload" in {
    Cmd(Opt("a"), Opt("b"))

    Cmd(Opt("a"), Opt("b"))("run")(Opt("x"), Opt("y"))
  }
