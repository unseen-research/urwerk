package urwerk.system

import urwerk.test.TestBase
import urwerk.io.Path
import urwerk.source.{Source, Singleton}
import urwerk.system.Process.Status.*
import java.io.IOException

class ExecTest extends TestBase:

  trait SrcOps[+A, +SS[_]]:
    self: Src[A] =>

    protected def fromSrc[B](src: Src[B]): SS[B] = ???

    def map[B](op: A => B): SS[B] =
      ???


  object Src:
    class Impl[+A] extends SrcOps[A, Src] with Src[A]

    def apply[A](elems: A*): Src[A] = ???

  trait Src[+A]:
    def map[B](op: A => B): Src[B]

  object Single:
    class Impl[+A] extends SrcOps[A, Single] with Single[A]

    def apply[A](elem: A): Single[A] = ???

  trait Single[+A] extends Src[A]:
    def map[B](op: A => B): Single[B]

  object Opt:
    def apply[A](elem: A): Opt[A] = ???

  trait Opt[+A] extends Src[A]:
    def map[B](op: A => B): Opt[B]


  "exec not existing" in {
    val exec = Path("/0815")
    intercept[IOException]{
      Exec(exec, "--version").process
        .block
    }
  }

  "exec status" in {
    val exec = Path(sys.props("java.home") + "/bin/java")
    val status = Exec(exec, "--version").process
      .flatMap(_.status)
      .toSeq.block

    val Running(runningProc) = status(0)
    val Terminated(terminatedProc, exitStatus) = status(1)

    println(s"RUN: $runningProc")
    println(s"Term: $terminatedProc")
  }