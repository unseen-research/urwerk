package urwerk.system

import java.io.InputStream

import java.nio.file.NoSuchFileException
import java.nio.file.Files
import java.nio.channels.Channels
import java.time.Instant
import java.lang.Process as JProcess
import java.lang.ProcessHandle as JProcessHandle
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.given
import scala.jdk.FutureConverters.given
import scala.jdk.OptionConverters.given
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext

import urwerk.io.file.Path
import urwerk.source.Singleton
import urwerk.source.Sink
import urwerk.source.Source
import urwerk.io.ByteString
import urwerk.io.Streams.*
import urwerk.system.Exec.NoSuchExecutableException
import urwerk.system.Exec.ExecutionException

import Process.*
import Exec.Factory

object Process extends Factory[Process]:

  object Output extends Factory[Either[ByteString, ByteString]]:
    type S[_] = Source[Either[ByteString, ByteString]]
    def fromExec(exec: Exec)(using executor: ExecutionContext): Source[Either[ByteString, ByteString]] =
      exec.connectErrorToOutput(false)
      .toProcess.flatMap{proc =>
        val err = proc.errorOutput
          .map(Left(_))

        val status = proc.status
          .filter(_ => false)
          .map[Either[ByteString, ByteString]](_ => ???)

        proc.output.map(Right(_))
          .mergeDelayError(32, err)
          .mergeDelayError(32, status)
      }

  trait OutputFactory extends Factory[ByteString]:
    type S[_] = Source[ByteString]

  object JointOutput extends OutputFactory:
    def fromExec(exec: Exec)(using executor: ExecutionContext): Source[ByteString] =
      outputSource(exec.connectErrorToOutput(true), executor)

  object StdOutput extends OutputFactory:
    def fromExec(exec: Exec)(using executor: ExecutionContext): Source[ByteString] =
      outputSource(exec.connectErrorToOutput(false), executor)

  object ErrOutput extends OutputFactory:
    def fromExec(exec: Exec)(using executor: ExecutionContext): Source[ByteString] =
      errorOutputSource(exec.connectErrorToOutput(false), executor)

  type S[_] = Singleton[Process]

  def fromExec(exec: Exec)(using executor: ExecutionContext): Singleton[Process] =
    exec.toProcess

  enum Status:
    case Running extends Status
    case Terminated(val code: Int) extends Status

  private[system] def processOf(proc: JProcess): Info =
    val info = proc.info
    val exec = info.command.orElse("")
    val args = info.arguments.orElse(Array.empty).to(Seq)
    val startInstant = info.startInstant.orElse(Instant.now)
    val user = info.user.orElse("")

    Info(exec, args, proc.pid, startInstant, user)

  case class Info(executable: String, arguments: Seq[String], pid: Long, startInstant: Instant, user: String)

object Exec:
  def apply(path: Path, args: String*): Exec = Exec(path, args, None, Map(), false)

  class NoSuchExecutableException(val path: Path, message: String) extends NoSuchFileException(message)

  class ExecutionException(val exitCode: Int) extends RuntimeException(s"Execution failed: exitCode=$exitCode")

  trait Factory[A]:
    type S[_]

    def fromExec(exec: Exec)(using executor: ExecutionContext): S[A]

  extension (exec: Exec)
    def toProcess(using executor: ExecutionContext): Singleton[Process] =
      processSource(exec, executor)

    def to[A](factory: Factory[A])(using executor: ExecutionContext): factory.S[A] =
      factory.fromExec(exec)

    def jointOutput(using executor: ExecutionContext): Source[ByteString] =
      outputSource(exec.connectErrorToOutput(true),
        executor)

    def output(using executor: ExecutionContext): Source[ByteString] = outputSource(exec, executor)

    def errorOutput(using executor: ExecutionContext): Source[ByteString] = errorOutputSource(exec, executor)

    def status(using executor: ExecutionContext): Source[Status] = statusSource(exec, executor)

case class Exec(path: Path, args: Seq[String], cwd: Option[Path], env: Map[String, String], connectErrorToOutput: Boolean):
  def arg(arg: String): Exec =
    copy(args = args :+ arg)

  def args(args: String*): Exec =
    copy(args = this.args ++ args)

  def param(name: String, value: String): Exec =
    val prefixedName = if name.startsWith("-") then name
      else if name.size == 1 then "-" + name
      else "--" +name
    copy(args = args ++ Seq(prefixedName, value))

  def connectErrorToOutput(connect: Boolean): Exec =
    copy(connectErrorToOutput = connect)

  def cwd(path: Path): Exec =
    copy(cwd = Some(path))

trait Process:
  def info: Process.Info
  def output: Source[ByteString]
  def errorOutput: Source[ByteString]
  def status: Source[Status]

private def statusSource(exec: Exec, ec: ExecutionContext): Source[Status] =
  startProc(exec) match
    case Success(proc) => createStatusSource(proc)
    case Failure(e) => Source.error(e)

private def outputSource(exec: Exec, ec: ExecutionContext): Source[ByteString] =
  startProc(exec) match
    case Success(proc) =>
      outputSource(proc,
        proc.getInputStream()
        .toSource)
    case Failure(error) => Source.error(error)

private def outputSource(proc: JProcess, source: Source[ByteString]) =
  source.concat(Source.create[ByteString]{sink =>
    val exitCode = proc.waitFor
    if exitCode == 0 then
      sink.complete()
    else
      sink.error(ExecutionException(exitCode))
  })

private def errorOutputSource(exec: Exec, ec: ExecutionContext): Source[ByteString] =
  startProc(exec) match
  case Success(proc) =>
      outputSource(proc,
        proc.getErrorStream()
        .toSource)
  case Failure(error) => Source.error(error)

private def processSource(exec: Exec, ec: ExecutionContext): Singleton[Process] =
  def start: Singleton[Process] =
    val procTry = startProc(exec).map{proc =>
      val info: Info = processOf(proc)
      new Process {
        val info = info
        def output: Source[ByteString] = proc.getInputStream()
          .toSource
          .subscribeOn(ec)
        def errorOutput: Source[ByteString] = proc.getErrorStream()
          .toSource
          .subscribeOn(ec)
        def status: Source[Status] = createStatusSource(proc)
      }

    }
    Singleton.from(procTry)

  Singleton.defer(start)

private def startProc(exec: Exec) =
  Try{
    val path = exec.path
    if !Files.isRegularFile(path) then
      throw NoSuchExecutableException(path, s"File does not exist: path=$path")
    else if !Files.isExecutable(path) then
      throw NoSuchExecutableException(path, s"File is not executable: path=$path")

    val cmd = exec.path.toString +: exec.args
    val procBuilder = ProcessBuilder(cmd.asJava)

    procBuilder
    .redirectErrorStream(exec.connectErrorToOutput)
    .start
  }

private def createStatusSource(jproc: JProcess): Source[Status] =
  Source.create[Status]{sink =>
    if !jproc.isAlive then
      sink.next(Status.Terminated(jproc.exitValue))
      sink.complete()
    else
      sink.next(Status.Running)
      while(jproc.isAlive)
        Thread.sleep(10)
      sink.next(Status.Terminated(jproc.exitValue))
      sink.complete()
  }
