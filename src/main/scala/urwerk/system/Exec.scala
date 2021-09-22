package urwerk.system

import java.time.Instant
import java.lang.Process as JProcess
import java.lang.ProcessHandle as JProcessHandle

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
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference

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
  def sdtOut: Source[String]
  def errOut: Source[String]
  def status: Source[Status]

extension (exec: Exec)
  def process(using executor: ExecutionContext): Singleton[ProcessInterface] =
    _process(exec, executor)

private def _process(executable: Exec, executor: ExecutionContext): Singleton[ProcessInterface] =
  val cmd = executable.path.toString +: executable.args
  val procBuilder = ProcessBuilder(cmd.asJava)

  case class Con
  def start: Singleton[ProcessInterface] =
    val procTry = Try{
      val jproc = procBuilder.start
      val proc: Process = processOf(jproc)

      val threadStarted = LinkedBlockingQueue[Boolean]
      val stdSink = AtomicReference[Option[Sink[String]]](None)
      val errSink = AtomicReference[Option[Sink[String]]](None)
      val statusSink = AtomicReference[Option[Sink[Status]]](None)

      // def run(): Unit = 
      //   while jproc.isAlive 

      //   if !jproc.isAlive then
      //     statusSink.get().foreach(
      //       _.next(Status.Terminated(proc, jproc.exitValue)))
      //     statusSink.complete()

      val statusSrc = Source.create[Status]{sink =>
        if jproc.isAlive then
          statusSink.set(Some(sink))
          if threadStarted.          }
        else
          sink.next(Status.Terminated(proc, jproc.exitValue))
          sink.complete()
      }
      new ProcessInterface {
        def sdtOut: Source[String] = ???
        def errOut: Source[String] = ???
        def status: Source[Status] = statusSrc
      }
    }

    Singleton.from(procTry)

  Singleton.defer(start)