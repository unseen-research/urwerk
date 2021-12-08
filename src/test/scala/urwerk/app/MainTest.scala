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
import urwerk.source.Source
import scala.runtime.TupleXXL
import scala.runtime.LazyRef

@Command(name = "Tets App", mixinStandardHelpOptions = true, version = Array("1.0.x"))
class TestMainCommand extends Callable[Int], Main.Application:
  @Option(names = Array("--global"), scope = ScopeType.INHERIT) // option is shared with subcommands
  var global: Int = 0

  override def toString = "test"

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

//given Seq[Main.Command] = Seq()



given ExitOp with
  def apply(status: Int): Unit = ()

class MainTest extends TestBase:

  case class Conf(a: String, b: Int)

  object PostConditions:
    opaque type WrappedResult[T] = T

    def result[T](using r: WrappedResult[T]): T = r

    extension [T](x: T)
      def ensuring(condition: WrappedResult[T] ?=> Boolean): T =
        assert(condition(using x))
        x
  end PostConditions
  import PostConditions.{ensuring, result}

  val s = List(1, 2, 3).sum.ensuring(result == 6)


  object Params
    //def param[A, B](): Param[A, B] = Param("")

  case class Param[A](name: String):
    def apply[B](using Cmd.GetConf[B])(op: (B, A) => B) = ???

  object Cmd:
    type GetConf[A] = () => A

  class Cmd[A](name: String):


    def apply(name: String): Cmd[A] = ???

    def context(params: Cmd.GetConf[A] ?=> Param[_]*): Cmd[A] = ???

    def params[P](params: Param[A]*): Cmd[A] = ???
    def x = ""


  "lazy" in {
    val l = LazyRef[String]()
    l.initialize("a")
    println(l.value)

    l.initialize("B")
    println(l.value)

  }

  "ccc" in {
    Cmd[Conf]("run")
      .context(
        Param[String]("y").apply((a, b) => a), Param("z"), Param[String]("y").apply((a, b) => a))
      .params(
        Param("y"), Param("z"))
  }



  "summon" in {
    given Main.Application = TestMainCommand()
    val x = summon[Main.Application]
    x.toString should be ("test")
  }

  "print help 2" in {
    var exitStatus = -1
    val outCapture: OutputStream = ByteArrayOutputStream()
    val errCapture = ByteArrayOutputStream()
    withOutput(outCapture){
      mainOp(Array("--help"))
    }

    println("==============================2")
    println(outCapture.toString)
    println("==============================")

  }
  "print help" in {
    val help = App.arg("--help").jointOutput.onErrorResume(e=>Source()).mkString.block

    println(s"============================")
    println(help)
  }

given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

val App = Exec(execPath)
  .param("--class-path", sys.props("java.class.path"))
  .arg("urwerk.app.Main")

def withOutput[T](out: OutputStream)(thunk: => T): T =
  val sysOut = System.out
  System.setOut(PrintStream(out))

  try
    Console.withOut(out){
      thunk
    }
  finally
    System.setOut(sysOut)

def withErrorOutput[T](out: OutputStream)(thunk: => T): T =
  val sysOut = System.err
  System.setErr(PrintStream(out))

  try
    Console.withOut(out){
      thunk
    }
  finally
    System.setErr(sysOut)
