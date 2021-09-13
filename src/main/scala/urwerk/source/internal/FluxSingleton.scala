package urwerk.source.internal

import reactor.core.publisher.Flux
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Singleton
import urwerk.source.SingletonFactory
import java.util.concurrent.CompletableFuture
import reactor.core.publisher.Mono

object FluxSingleton extends SingletonFactory:

  def apply[A](elem: A): Singleton[A] =
    wrap(Flux.just(elem))

  def defer[A](op: => Singleton[A]): Singleton[A] =
    wrap(Flux.defer(() =>
      op.toFlux))

  def error[A](error: Throwable): Singleton[A] =
    wrap(Flux.error(error))

  def from[A](future: CompletableFuture[A]): Singleton[A] =
    wrap(
      Mono.fromFuture(future)
        .flux())

  private[source] def wrap[A](flux: Flux[A]): Singleton[A] =
    new Singleton[A]{
      val fluxSingleton = new FluxSingleton(flux)
      export fluxSingleton.*
    }

class FluxSingleton[+A](flux: Flux[_<: A]) extends FluxSource[A](flux):
  def block: A =
    flux.blockFirst()

  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(flux.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Singleton[B]): Singleton[B] =
    Singleton.wrap(
      flux.flatMap(elem =>
        op(elem).toFlux))

  override def map[B](op: A => B): Singleton[B] =
    Singleton.wrap(flux.map(op(_)))
