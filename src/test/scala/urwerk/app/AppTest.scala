package urwerk.app

import urwerk.test.TestBase
import urwerk.source.Source
import urwerk.io.ByteString
import urwerk.io.file.Path
import urwerk.source.Singleton
import java.time.Instant
import scala.concurrent.duration.Duration
import urwerk.system.Exec
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import urwerk.app.command.Command
import urwerk.app.command.Parameters
import urwerk.io
import scala.util.Random
import urwerk.app.App.ExitStatus

class AppTest extends TestBase:

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "exit status unapply" in {
    new IllegalArgumentException("exit status message") with ExitStatus(77) match
      case error@ExitStatus(status) =>
        error.getMessage should be ("exit status message")
        status should be(77)
  }

  "run evaluate output" in {
    val output = javaExec.args("arg1", "arg2").output.mkString.block
    output should be (s"Out: arg1,Out: arg2,")
  }

  "run evaluate error output" in {
    val output = javaExec.args("arg1", "arg2").errorOutput.mkString.block
    output should be (s"Err: arg1,Err: arg2,")
  }

  "run evaluate joint output" in {
    val output = javaExec.args("arg1", "arg2").jointOutput.mkString.block
    output should be (s"Out: arg1,Err: arg1,Out: arg2,Err: arg2,")
  }

val nl = System.lineSeparator

val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

val javaExec = Exec(execPath)
  .param("--class-path", sys.props("java.class.path"))
  .arg("urwerk.app.TestApp")

val params = Parameters[Seq[String]]
import params.*

def mainSrc(args: Seq[String]) =
  Source.from(args.flatMap(arg => Seq(Right(s"Out: $arg,"), Left(s"Err: $arg,"))))
    .map(_.map(ByteString.from)
      .left
      .map(ByteString.from))

object TestApp extends App(mainSrc)
