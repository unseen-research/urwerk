package urwerk.system

import java.io.IOException
import java.nio.file
import java.util.concurrent.Executors
import java.nio.file.NoSuchFileException

import scala.concurrent.ExecutionContext
import scala.io.Codec

import urwerk.test.*
import urwerk.io.file.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Output
import urwerk.system.Process.ErrOutput
import urwerk.system.Process.JointOutput
import urwerk.system.Process.StdOutput
import urwerk.system.Process.Status.*
import urwerk.system.Process.Status
import urwerk.system.Exec.NoSuchExecutableException
import urwerk.system.Exec.ExecutionException

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
    intercept[NoSuchExecutableException]{
      Exec(path, "--version").toProcess
        .block
    }
  }

  "exec not executable" in {
    val path = uniqueFile
    intercept[NoSuchExecutableException]{
      Exec(path, "--version").toProcess
        .block
    }
  }

  "exec process status" in {
    val status = exec.arg("42").toProcess
      .flatMap(_.status)
      .toSeq.block

    val Status.Running = status(0)
    val Status.Terminated(statusCode) = status(1)
    statusCode should be (42)
  }

  "exec process std out" in {
    val out = exec.args("0", "abc", "3", "err", "3").toProcess
      .flatMap(_.output)
      .mkString.block

    out should be(s"abc${nl}abc${nl}abc${nl}")
  }

  "exec process error output" in {
    val out = exec.args("0", "abc", "3", "xyz", "3").toProcess
      .flatMap(_.errorOutput)
      .mkString.block

    out should be(s"xyz${nl}xyz${nl}xyz${nl}")
  }

  "exec process connect error to output" in {
    val out = exec.args("0", "abc", "3", "xyz", "3")
      .connectErrorToOutput(true)
      .toProcess
      .flatMap(proc => Source(proc.output, proc.errorOutput).concat)
      .mkString.block

    out should be(s"abc${nl}xyz${nl}abc${nl}xyz${nl}abc${nl}xyz${nl}")
  }

  "exec to process" in {
    val status = exec.arg("42").to(Process)
      .flatMap(_.status)
      .toSeq.block

    val Status.Running = status(0)
    val Status.Terminated(statusCode) = status(1)
    statusCode should be (42)
  }

  "exec to output" in {
    import urwerk.io.ByteString
    val out = exec.args("0", "abc", "3", "xyz", "3")
      .to(Output)
      .foldLeft(("", "")){
        case ((left, right), Right(bytes)) =>
          (left, right + bytes)
        case ((left, right), Left(bytes)) =>
          (left + bytes, right)
      }
      .block

    out should be(
      (s"xyz${nl}xyz${nl}xyz${nl}", s"abc${nl}abc${nl}abc${nl}")
    )
  }

  "exec to output with error code" in {
    import urwerk.io.ByteString
    val out = exec.args("7", "abc", "3", "xyz", "3")
      .to(Output)
      .foldLeft(("", "")){
        case ((left, right), Right(bytes)) =>
          (left, right + bytes)
        case ((left, right), Left(bytes)) =>
          (left + bytes, right)
      }
      //onErrorResume{case e => Source()}.block

    out should be(
      (s"xyz${nl}xyz${nl}xyz${nl}", s"abc${nl}abc${nl}abc${nl}")
    )
  }

  "exec to joint output" in {
    val out = exec.args("0", "abc", "3", "xyz", "3")
      .to(JointOutput)
      .mkString.block

    out should be(s"abc${nl}xyz${nl}abc${nl}xyz${nl}abc${nl}xyz${nl}")
  }

  "exec to std output" in {
    val out = exec.args("0", "abc", "3", "err", "3").to(StdOutput)
      .mkString.block

    out should be(s"abc${nl}abc${nl}abc${nl}")
  }

  "exec output executable not exists" in {
    val path = Path("/0815")
    intercept[NoSuchExecutableException]{
      Exec(path, "--version")
        .output.last.block
    }
  }

  "exec output not executable" in {
    val path = uniqueFile
    intercept[NoSuchExecutableException]{
      Exec(path, "--version")
        .output.last.block
    }
  }

  "exec output fails with non zero exit code" in {
    val ex = intercept[ExecutionException]{
      exec.args("77", "abc", "3", "err", "3")
        .output.last.block
    }
    ex.exitCode should be (77)
  }

  "exec error output connected to output" in {
    val ex = intercept[ExecutionException]{
      exec.connectErrorToOutput(true)
        .args("77", "abc", "3", "err", "3")
        .output.last.block
    }
    ex.exitCode should be (77)
  }

  "exec error output" in {
    val out = exec.args("0", "abc", "3", "xyz", "3").errorOutput
      .mkString.block

    out should be(s"xyz${nl}xyz${nl}xyz${nl}")
  }

  "exec error output executable not exists" in {
    val path = Path("/0815")
    intercept[NoSuchExecutableException]{
      Exec(path, "--version")
        .errorOutput.last.block
    }
  }

  "exec error output not executable" in {
    val path = uniqueFile
    intercept[NoSuchExecutableException]{
      Exec(path, "--version")
        .errorOutput.last.block
    }
  }

  "exec error output fails with non zero exit code" in {
    intercept[ExecutionException]{
      exec.args("77", "abc", "3", "err", "3")
        .errorOutput.last.block
    }
  }

  "exec status" in {
    val status = exec.args("44", "abc", "3", "xyz", "3").status
      .toSeq.block

    val Status.Running = status(0)
    val Status.Terminated(statusCode) = status(1)
    statusCode should be (44)
  }

  val nl = System.lineSeparator

  val execPath = Path(sys.props("java.home") + "/bin/java").toAbsolutePath

  val exec = Exec(execPath)
    .param("--class-path", sys.props("java.class.path"))
    .arg("urwerk.system.TestMain")
