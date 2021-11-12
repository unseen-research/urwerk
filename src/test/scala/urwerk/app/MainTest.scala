package urwerk.app

import urwerk.test.TestBase
import picocli.CommandLine
import picocli.CommandLine.Command
import picocli.CommandLine.Option
import picocli.CommandLine.Parameters
import java.util.concurrent.Callable
import picocli.CommandLine.ScopeType
import picocli.CommandLine.ArgGroup

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

class ModuleSpec:
  @Option(names = Array("--module"), required = true)
  var module: String = "default mod"

  @Option(names = Array("--version"), required = true)
  var version: String = "default-ver"

@Command(name = "run", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class Run extends Callable[Int]:
  @ArgGroup(exclusive = false, multiplicity = "0-1")
  var modSpec: ModuleSpec = ModuleSpec()

  @Parameters(index = "0", arity = "0-1")
  var module: String = ""

  @Parameters(index = "1", arity = "0..*")
  var args: java.util.List[String] = java.util.ArrayList()

    def call(): Int =
      println(s"RESULT ${modSpec.module} ${modSpec.version} ${module} ${args}")

      789

class MainTest extends TestBase:

  "run help" in {
    val res = new CommandLine(UrwerkApp())
      .addSubcommand(Run())
      .setStopAtPositional(true)
      .execute("run", "abc", "--module")
    println(s"RES $res")

  }


  "run 1" in {
    val res = new CommandLine(UrwerkApp())
      .addSubcommand(Run())
      .setStopAtPositional(true)
      .setStopAtUnmatched(true)
      .execute("--global", "786", "run", "--module", "the module", "--version", "3.3.4", "dfdf", "dkfjdla")
    println(s"RES $res")

  }

  "run 2" in {
    val res = new CommandLine(UrwerkApp())
      .addSubcommand(Run())
      .setStopAtPositional(true)
      .setStopAtUnmatched(true)
      .execute("--global", "786", "run", "dfdf", "dkfjdla")
    println(s"RES $res")

  }


  // "run 2" in {
  //   val res = new CommandLine(UrwerkApp())
  //     .setStopAtPositional(true)
  //     .setStopAtUnmatched(true)
  //     .addSubcommand(Run())
  //     .execute("--global", "444", "run", "ur.urwerk.module:1.2.3", "--arg1", "the module")
  //   println(s"RES $res")

  // }
