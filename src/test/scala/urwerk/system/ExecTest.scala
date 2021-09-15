package urwerk.system

import urwerk.test.TestBase
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import java.io.IOException

class ExecTest extends TestBase:

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