package urwerk.system

import java.io.IOException
import java.nio.file
import java.util.concurrent.Executors

import urwerk.test.*
import urwerk.io.file.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import scala.concurrent.ExecutionContext
import scala.io.Codec
import urwerk.system.Process.Status

class ExecTest extends TestBase:
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  "args" in {
    val exec = Exec(uniqueFile, "abc")
      .arg("def")
      .args("ghi", "hkl")
    exec.args should be (Seq("abc", "def", "ghi", "hkl"))
  }

  "param with plain name" in {
    val exec = Exec(uniqueFile, "abc")
      .param("param-name", "param-value")

    exec.args should be (Seq("abc", "--param-name", "param-value"))
  }

  "param with shortcut" in {
    val exec = Exec(uniqueFile, "abc")
      .param("p", "param-value")

    exec.args should be (Seq("abc", "-p", "param-value"))
  }

  "param with dashed name" in {
    val exec = Exec(uniqueFile, "abc")
      .param("-param-name", "param-value")

    exec.args should be (Seq("abc", "-param-name", "param-value"))
  }

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

    val Status.Running = status(0)
    val Status.Terminated(statusCode) = status(1)
    statusCode should be (42)
  }

  "exec process stdout" in {
    val stdOut = exec.args("0", "abc", "3", "err", "3").process
      .flatMap(_.output)
      .mkString.block

    stdOut should be(s"abc${nl}abc${nl}abc${nl}")
  }

  "exec process error output" in {
    val stdOut = exec.args("0", "abc", "3", "xyz", "3").process
      .flatMap(_.errorOutput)
      .mkString.block

    stdOut should be(s"xyz${nl}xyz${nl}xyz${nl}")
  }

  val nl = System.lineSeparator

  val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

  val exec = Exec(execPath)
    .param("--class-path", sys.props("java.class.path"))
    .arg("urwerk.system.TestMain")
