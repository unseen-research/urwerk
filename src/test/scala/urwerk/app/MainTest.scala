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


// @ArgGroup(exclusive = false)
//     Dependent dependent;

//     static class Dependent {
//         @Option(names = "-a", required = true) int a;
//         @Option(names = "-b", required = true) int b;
//         @Option(names = "-c", required = true) int c;
//     }

class RunOptions:

  @Option(names = Array("--module"))
  var module: String = "default mod"

  @Option(names = Array("--version"))
  var version: String = "default-ver"

  @Parameters(index = "0", arity = "0..*")
  var args: java.util.List[String] = java.util.ArrayList()

class RunOptions2:
  @Parameters(index = "0", arity = "1")
  var module: java.util.List[String] = java.util.ArrayList()

  @Parameters(index = "1", arity = "0..*")
  var args: java.util.List[String] = java.util.ArrayList()

@Command(name = "run", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class Run extends Callable[Int]:

    @ArgGroup(exclusive = true)
    var options: RunOptions = RunOptions()

    @ArgGroup(exclusive = true)
    var options2: RunOptions2 = RunOptions2()

    def call(): Int =
      println(s"VARIANTE1 ${options.module} ${options.version} ${options.args}")

      println(s"VARIANTE2 ${options2.module}  ${options2.args}")

      789

class MainTest extends TestBase:

  "run 1" in {
    val res = new CommandLine(UrwerkApp())
      .setStopAtPositional(true)
      .setStopAtUnmatched(true)
      .addSubcommand(Run())
      .execute("--global", "444", "run", "--module", "the module", "--version", "3.3.4", "arg0")
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
