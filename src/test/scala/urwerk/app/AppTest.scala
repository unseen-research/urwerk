package urwerk.app

import urwerk.test.TestBase
import urwerk.source.Source
import urwerk.io.file.Path
import urwerk.source.Singleton
import java.time.Instant
import scala.concurrent.duration.Duration
import urwerk.system.Exec
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

class AppTest extends TestBase:
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "run" in {
    val output = javaExec.jointOutput.mkString.block
    output should be (s"Greetings${nl}Mortal${nl}")
  }

val nl = System.lineSeparator

val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

val javaExec = Exec(execPath)
  .param("--class-path", sys.props("java.class.path"))
  .arg("urwerk.app.TestApp")

object TestApp extends App(Seq())
