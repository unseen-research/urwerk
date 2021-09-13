package urwerk.source

import urwerk.source.internal.FluxOptional

object Optional extends OptionalFactory:
  export FluxOptional.*

trait Optional[+A] extends Source[A]:
  def block: Option[A]

  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Optional[B]): Optional[B]

  def map[B](op: A => B): Optional[B]

trait OptionalFactory:
  def apply[A](elem: A): Optional[A]

  def apply[A](): Optional[A]

  def apply[A](elemOpt: Option[A]): Optional[A]

  def empty[A]: Optional[A]

  def error[A](error: Throwable): Optional[A]
