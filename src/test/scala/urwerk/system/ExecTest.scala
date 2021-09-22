package urwerk.system

import java.io.IOException

import urwerk.test.TestBase
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

class ExecTest extends TestBase:
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "exec not existing" in {
    val exec = Path("/0815")
    intercept[IOException]{
      Exec(exec, "--version").process
        .block
    }
  }

  "exec status" in {
    val exec = Path(sys.props("java.home") + "/bin/java")
    val status = Exec(exec, "--version").process
      .flatMap(_.status)
      .toSeq.block

    val Running(runningProc) = status(0)
    val Terminated(terminatedProc, exitStatus) = status(1)

    println(s"RUN: $runningProc")
    println(s"Term: $terminatedProc")
  }