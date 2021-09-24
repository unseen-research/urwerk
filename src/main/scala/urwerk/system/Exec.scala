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

import urwerk.io.Path
import urwerk.source.Singleton
import urwerk.source.Sink
import urwerk.source.Source
import scala.concurrent.Promise
import scala.util.Try
import urwerk.source.BufferOverflowStrategy
import urwerk.system.Process.Status
import scala.concurrent.ExecutionContext

import Process.*
import urwerk.io.ByteString
import urwerk.io.Streams.given

object Process:
  object Out:
    def unapply(proc: Process): Option[(Source[String], Source[String])] =
        //Some((proc.sdtOut, proc.errOut))
        ???

  enum Status:
    val process: Process

    case Running(process: Process) extends Status
    case Terminated(process: Process, exitStatus: Int) extends Status

  private[system] def processOf(proc: JProcess): Process =
    val info = proc.info
    val exec = info.command.orElse("")
    val args = info.arguments.orElse(Array.empty).to(Seq)
    val startInstant = info.startInstant.orElse(Instant.now)
    val user = info.user.orElse("")

    Process(exec, args, proc.pid, startInstant, user)

case class Process(executable: String, arguments: Seq[String], pid: Long, startInstant: Instant, user: String)

object Exec:
  def apply(path: Path, args: String*): Exec = Exec(path, args, None, Map())

case class Exec(path: Path, args: Seq[String], cwd: Option[Path], env: Map[String, String]):
  def arg(arg: String): Exec =
    copy(args = args :+ arg)

  def args(arg: String*): Exec =
    copy(args = args ++ args)

  def param(name: String, value: String): Exec =
    copy(args = args ++ Seq(name, value))

  def cwd(path: Path): Exec =
    copy(cwd = Some(path))

trait ProcessInterface:
  def output: Source[ByteString]
  def errorOutput: Source[ByteString]
  def status: Source[Status]

extension (exec: Exec)
  def process(using executor: ExecutionContext): Singleton[ProcessInterface] =
    _process(exec, executor)

  def output(using executor: ExecutionContext): Singleton[ProcessInterface] = ???

  def errorOutput(using executor: ExecutionContext): Singleton[ProcessInterface] = ???

private def _process(executable: Exec, ec: ExecutionContext): Singleton[ProcessInterface] =
  val cmd = executable.path.toString +: executable.args
  val procBuilder = ProcessBuilder(cmd.asJava)

  def start: Singleton[ProcessInterface] =
    val procTry = Try{
      val jproc = procBuilder.start

      new ProcessInterface {
        def output: Source[ByteString] = jproc.getInputStream()
          .toSource
          .subscribeOn(ec)
        def errorOutput: Source[ByteString] = jproc.getErrorStream()
          .toSource
          .subscribeOn(ec)
        def status: Source[Status] = createStatusSource(jproc)
      }
    }

    Singleton.from(procTry)

  Singleton.defer(start)

private def createStatusSource(jproc: JProcess): Source[Status] =
  val proc: Process = processOf(jproc)

  Source.create[Status]{sink =>
    if !jproc.isAlive then
      sink.next(Status.Terminated(proc, jproc.exitValue))
      sink.complete()
    else
      sink.next(Status.Running(proc))
      while(jproc.isAlive)
        Thread.sleep(10)
      sink.next(Status.Terminated(proc, jproc.exitValue))
      sink.complete()
  }
