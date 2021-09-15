package urwerk.source.internal

import java.util.concurrent.CompletableFuture

import reactor.core.publisher.Mono
import reactor.core.publisher.Flux

import scala.concurrent.Future
import scala.jdk.FutureConverters.given

import urwerk.source.Source
import urwerk.source.Optional
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Singleton
import urwerk.source.SingletonFactory
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import urwerk.source.SourceException
import java.io.IOException

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

  def from[A](future: Future[A]): Singleton[A] =
    from(
      future.asJava.toCompletableFuture)

  def from[A](elemTry: Try[A]): Singleton[A] = elemTry match
    case Success(elem) =>
      Singleton(elem)
    case Failure(e) =>
      Singleton.error(e)

  private[source] def wrap[A](flux: Flux[A]): Singleton[A] =
    new Singleton[A]{
      val fluxSingleton = new FluxSingleton(flux)
      export fluxSingleton.*
    }

class FluxSingleton[+A](flux: Flux[_<: A]) extends FluxSource[A](flux):
  def block: A =
    stripReactiveException(flux.blockFirst())

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
