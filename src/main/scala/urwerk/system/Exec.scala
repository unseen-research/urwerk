package urwerk.system

import java.io.InputStream
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
import scala.concurrent.Promise
import scala.util.Try
import scala.concurrent.ExecutionContext

import urwerk.io.file.Path
import urwerk.source.Singleton
import urwerk.source.Sink
import urwerk.source.Source
import urwerk.io.ByteString
import urwerk.io.Streams.given

import Process.*
import scala.util.Success
import scala.util.Failure

object Process:
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

extension (exec: Exec)
  def process(using executor: ExecutionContext): Singleton[Process] =
    processSource(exec, executor)

  def output(using executor: ExecutionContext): Source[ByteString] = outputSource(exec, executor)

  def errorOutput(using executor: ExecutionContext): Source[ByteString] = errorOutputSource(exec, executor)

private def outputSource(exec: Exec, ec: ExecutionContext): Source[ByteString] =
  startProc(exec).map(_.getInputStream().toSource) match
    case Success(source) => source
    case Failure(error) => ???

private def errorOutputSource(exec: Exec, ec: ExecutionContext): Source[ByteString] =
  startProc(exec).map(_.getErrorStream().toSource) match
  case Success(source) => source
  case Failure(error) => ???

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
  val cmd = exec.path.toString +: exec.args
  val procBuilder = ProcessBuilder(cmd.asJava)
  Try{
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
