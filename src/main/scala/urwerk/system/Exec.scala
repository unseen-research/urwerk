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
import urwerk.source.Source
import scala.concurrent.Promise

object Process:
  object Out:
    def unapply(proc: Process): Option[(Source[String], Source[String])] =
        //Some((proc.sdtOut, proc.errOut))
        ???

  enum Status:
    def info: Info

    case Running(info: Info) extends Status
    case Terminated(info: Info, exitStatus: Int) extends Status

  case class Info(executable: String, arguments: Seq[String], pid: Long, startInstant: Instant, user: String)

  private def infoOf(proc: JProcess): Info =
    val info = proc.info
    val exec = info.command.toScala.getOrElse("")
    val args = info.arguments.toScala.map(_.to(Seq)).getOrElse(Seq())
    val startInstant = info.startInstant.toScala.getOrElse(Instant.now)
    val user = info.user.toScala.getOrElse("")

    Info(exec, args, proc.pid, startInstant, user)

object Exec:
  def apply(path: Path, args: String*): Exec = Exec(path, args, None, Map())

case class Exec(path: Path, args: Seq[String], cwd: Option[Path], env: Map[String, String])

class Process(proc: JProcess):
  import Process.*

  def sdtOut: Source[String] = ???

  def errOut: Source[String] = ???

  def status: Source[Status] =
    Source(statusOf(proc))
      .flatMap{
        case status: Status.Running =>
          Source(
              Source(status), Singleton.from(proc.onExit.thenApply(statusOf(_))))
            .concat
        case status: Status.Terminated =>
          Source(status)
      }

  def info: Info = infoOf(proc)

  private def statusOf(proc: JProcess): Status =
    if proc.isAlive then
      Status.Running(info)
    else
      Status.Terminated(info, proc.exitValue)

extension (exec: Exec)

  def apply(input: Source[String]): Singleton[Process] = process(input)

  def process(input: Source[String]): Singleton[Process] = ???

  def apply(): Singleton[Process] = process

  def process: Singleton[Process] =

    val cmd = exec.path.toString +: exec.args

    Singleton.defer(
        Singleton(ProcessBuilder(cmd.asJava).start))
      .map(new Process(_))
