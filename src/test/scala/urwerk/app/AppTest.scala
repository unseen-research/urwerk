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

val out = Source[ByteString]()
val err = Source[ByteString]()

class AppTest extends TestBase:
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "run" in {
    val output = javaExec.param("any-param", "XX***********XX").jointOutput.mkString.block
    output should be (s"Greetings${nl}Mortal${nl}")
  }

val nl = System.lineSeparator

val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

val javaExec = Exec(execPath)
  .param("--class-path", sys.props("java.class.path"))
  .arg("urwerk.app.TestApp")

val params = Parameters[Seq[String]]
import params.*

def mainSrc(in: Source[ByteString]) =
  Source(Right("abc"), Left("ABC"), Right("def"), Left("DEF"))
    .map(_.map(ByteString.from)
      .left
      .map(ByteString.from))

object TestApp extends App(mainSrc)
