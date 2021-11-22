package urwerk.app

import java.util.concurrent.Callable
import java.util.concurrent.Executors

import picocli.CommandLine
import picocli.CommandLine.Command
import picocli.CommandLine.Option
import picocli.CommandLine.Parameters
import picocli.CommandLine.ScopeType
import picocli.CommandLine.ArgGroup

import scala.concurrent.ExecutionContext

import urwerk.io.file.Path
import urwerk.test.TestBase
import urwerk.system.Exec
import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.PrintStream

@Command(name = "Tets App", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class MainCommmand extends Callable[Int]:
  @Option(names = Array("--global"), scope = ScopeType.INHERIT) // option is shared with subcommands
  var global: Int = 0

  def call(): Int =
    7

@Command(name = "run", mixinStandardHelpOptions = true)
class Run(exitStatus: Int = 0) extends Callable[Int]:
  @Option(names = Array("module"))
  var module: String = ""

  def call(): Int =
    exitStatus
end Run

@Command(name = "install", mixinStandardHelpOptions = true)
class Install(exitStatus: Int = 0) extends Callable[Int]:
  @Option(names = Array("module"))
  var module: String = ""

  def call(): Int =
    exitStatus

class MainTest extends TestBase:
  "ph" in {
    var exitStatus = -1
    val outCapture: OutputStream = ByteArrayOutputStream()
    val errCapture = ByteArrayOutputStream()
    val main = new Main(MainCommmand(), Run(), Install()){
        override def exit(status: Int) = exitStatus = status}

    withOutput(outCapture){
      println("xyz")
      main.main(Array("--help"))
    }

    println("==============================")
    println(outCapture.toString)
    println("==============================")

  }
  "print help" in {
    val help = App.arg("--help").jointOutput.mkString.block

    println(s"============================")
    println(help)
  }

given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

val App = Exec(execPath)
  .param("--class-path", sys.props("java.class.path"))
  .arg("urwerk.app.Main")


def withOutput[T](out: OutputStream)(thunk: => T): T =
  val savedOut = System.out
  System.setOut(PrintStream(out))
  try
    thunk
  finally
    System.setOut(savedOut)
