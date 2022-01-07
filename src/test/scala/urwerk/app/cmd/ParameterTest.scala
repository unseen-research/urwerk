package urwerk.app.cmd

import urwerk.test.TestBase
import scala.annotation.meta.param

object ParameterTest:
  opaque type WrappedResult[T] = T

  def wrap[A](value: A): WrappedResult[A] = value

class ParameterTest extends TestBase:

  import ParameterTest.*

  val result: Either[String,Int] = Right(5) //Either is the Parameter pendent to be created

  trait Monad[A[* <: Either[?, ?]]]:                        //some factory to create param eventually
    def onApply[B <: Either[?, ?]](f: B => Unit): A[B] = ???

  object EitherMonad:                       //incomplete builder called Param
    def apply[A]: EitherMonad[A] = ???

  class EitherMonad[T] extends Monad[[E] =>> Either[T, E]]

  val xx = EitherMonad[String]

  val xs: Either[String, Int] = EitherMonad[String].onApply(x => ())

