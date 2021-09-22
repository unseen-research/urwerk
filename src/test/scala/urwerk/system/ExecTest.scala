package urwerk.system

import java.io.IOException
import java.nio.file
import java.util.concurrent.Executors

import urwerk.test.TestBase
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import scala.concurrent.ExecutionContext


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

  "exec stdout" in {
    val exec = Path(sys.props("java.home") + "/bin/java")
    val classPath = sys.props("java.class.path")

    val status = Exec(exec, "--class-path", classPath, "urwerk.system.TestMain", "abcdefg", "xyz").process
      .flatMap(_.status)
      .toSeq.block

    println(s"SAT: $status")
  }