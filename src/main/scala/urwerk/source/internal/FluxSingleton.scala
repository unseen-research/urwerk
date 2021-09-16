package urwerk.source.internal

import java.util.concurrent.CompletableFuture

import reactor.core.publisher.Mono
import reactor.core.publisher.Flux

import scala.concurrent.Future
import scala.jdk.FutureConverters.given
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import urwerk.source.Source
import urwerk.source.Optional
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Singleton
import urwerk.source.SingletonFactory
import urwerk.source.SourceException
import scala.collection.View.Single

private[source] object FluxSingleton extends SingletonFactory:

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

  private[internal] def wrap[A](flux: Flux[A]): Singleton[A] =
    val fluxSingleton = new FluxSingleton(flux)
    new Singleton[A]{
      export fluxSingleton.*
    }

private class FluxSingleton[+A](flux: Flux[? <: A]) extends FluxSourceOps[A](flux), Singleton[A]:
  type S[A] = Singleton[A]

  protected def wrap[B](flux: Flux[? <: B]): Singleton[B] = FluxSingleton.wrap(flux)

  def block: A =
    stripReactiveException(flux.blockFirst())

  def filter(pred: A => Boolean): Optional[A] =
    FluxOptional.wrap(flux.filter(pred(_)))

  def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))
