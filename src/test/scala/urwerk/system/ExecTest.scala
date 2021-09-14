package urwerk.system

import urwerk.test.TestBase
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*

class ExecTest extends TestBase:
  "exec process" in {
    val javaExetutable = Path(sys.props("java.home") + "/bin/java")
    val status = Exec(javaExetutable, "--version").process
      .flatMap(_.status)
      .toSeq.block

    val Running(runningProc) = status(0)
    val Terminated(terminatedProc, exitStatus) = status(1)

    println(s"RUN: $runningProc")
    println(s"Term: $terminatedProc")
  }