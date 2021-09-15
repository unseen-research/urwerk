package urwerk.source.internal

import reactor.core.publisher.Flux
import urwerk.source.Optional
import urwerk.source.reactor.FluxConverters.*
import scala.jdk.OptionConverters.*
import urwerk.source.OptionalFactory

object FluxOptional extends OptionalFactory:
  def apply[A](elem: A): Optional[A] =
    wrap(Flux.just(elem))

  def apply[A](): Optional[A] =
    wrap(Flux.empty())

  def apply[A](elemOpt: Option[A]): Optional[A] =
    elemOpt match
      case Some(elem) => apply(elem)
      case None => apply()

  def empty[A]: Optional[A] =
    wrap(Flux.empty())

  def error[A](error: Throwable): Optional[A] =
    wrap(Flux.error(error))

  private[source] def wrap[B](flux: Flux[B]): Optional[B] =
    new FluxOptional(flux)

class FluxOptional[+A](flux: Flux[_<: A]) extends FluxSource[A](flux) with Optional[A]:
  def block: Option[A] =
    stripReactiveException(
      flux.next.blockOptional.toScala)

  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(flux.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Optional[B]): Optional[B] =
    Optional.wrap(
      flux.flatMap(elem =>
        op(elem).toFlux))

  override def map[B](op: A => B): Optional[B] =
    Optional.wrap(flux.map(op(_)))

