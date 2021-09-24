package urwerk.system

import java.io.IOException
import java.nio.file
import java.util.concurrent.Executors

import urwerk.test.*
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import scala.concurrent.ExecutionContext

class ExecTest extends TestBase:
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "exec not existing" in {
    val path = Path("/0815")
    intercept[IOException]{
      Exec(path, "--version").process
        .block
    }
  }

  "exec process status" in {
    val status = exec.arg("42").process
      .flatMap(_.status)
      .toSeq.block

    val Running(running) = status(0)
    val Terminated(terminated, exitStatus) = status(1)

    running should be (terminated)
    exitStatus should be (42)
    running.executable should be (execPath.toString)
  }

  "exec process stdout" in {
    val stdOut = exec.args("0", "abc", "3", "err", "3").process
    //   .flatMap(_.sdtOut)
    //   .mkString
    //   .block
    // stdOut should be("abcabcabc")
  }

  val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolute
  val exec = Exec(execPath)
    .param("--class-path", sys.props("java.class.path"))
    .arg("urwerk.system.TestMain")
