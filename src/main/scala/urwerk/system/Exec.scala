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
import java.util.concurrent.atomic.AtomicReference
import urwerk.system.Process.Status

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
    val exec = info.command.get
    val args = info.arguments.get.to(Seq)
    val startInstant = info.startInstant.get
    val user = info.user.get

    Process(exec, args, proc.pid, startInstant, user)

case class Process(executable: String, arguments: Seq[String], pid: Long, startInstant: Instant, user: String)

object Exec:
  def apply(path: Path, args: String*): Exec = Exec(path, args, None, Map())

case class Exec(path: Path, args: Seq[String], cwd: Option[Path], env: Map[String, String])

trait ProcessInterface(proc: JProcess):
  def sdtOut: Source[String]
  def errOut: Source[String]
  def status: Source[Status]
  //   Source(statusOf(proc))
  //     .flatMap{
  //       case status: Status.Running =>
  //         Source(
  //             Source(status), Singleton.from(proc.onExit.thenApply(statusOf(_))))
  //           .concat
  //       case status: Status.Terminated =>
  //         Source(status)
  //     }

  // private val process: Process = processOf(proc)

  // private def statusOf(proc: JProcess): Status =
  //   if proc.isAlive then
  //     Status.Running(process)
  //   else
  //     Status.Terminated(process, proc.exitValue)

extension (exec: Exec)

  def apply(input: Source[String]): Singleton[ProcessInterface] = process(input)

  def process(input: Source[String]): Singleton[ProcessInterface] = ???

  def apply(): Singleton[ProcessInterface] = process

  def process: Singleton[ProcessInterface] =
    val cmd = exec.path.toString +: exec.args
    val procBuilder = ProcessBuilder(cmd.asJava)

    def start: Singleton[ProcessInterface] =
      val procTry = Try(
        val proc = procBuilder.start

        val statusSrc = Source.create[Status]{sink =>

        }
        new ProcessInterface{
          def sdtOut: Source[String] = ???
          def errOut: Source[String] = ???
          def status: Source[Status] = statusSrc
        }

      Singleton.from(procTry)

    Singleton.defer(start)
