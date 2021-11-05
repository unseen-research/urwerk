package urwerk.app

import urwerk.test.TestBase
import picocli.CommandLine
import picocli.CommandLine.Command
import picocli.CommandLine.Option
import picocli.CommandLine.Parameters
import java.util.concurrent.Callable
import picocli.CommandLine.ScopeType

@Command(name = "app", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class UrwerkApp extends Callable[Int]:

    //@Option(names = Array("--global"))
    var global: Int = 0

    @Option(names = Array("--global"), scope = ScopeType.INHERIT) // option is shared with subcommands
    def setGlobal(value: Int) =
        println(s"SET GLOBAL $value")

    def call(): Int =
      println(s"Global $global")
      47


@Command(name = "run", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class Run extends Callable[Int]:

    @Option(names = Array("--module"))
    var module: String = ""

    def call(): Int =
      println(s"Module $module")
      777

class MainTest extends TestBase:

  "test command" in {

    val res = new CommandLine(new Run())
      .execute("-x", "77")
    println(s"RES $res")
  }

  "help" in {

    val res = new CommandLine(new Run())
      .execute("--version")
    println(s"RES $res")
  }

  "run" in {
    val res = new CommandLine(UrwerkApp())
      .addSubcommand(Run())
      .execute("--global", "444", "run", "--module", "the module")
    println(s"RES $res")

  }
