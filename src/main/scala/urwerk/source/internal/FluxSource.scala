package urwerk.source.internal

import java.util.function.{BiConsumer, BiFunction}
import java.util.concurrent.Flow

import org.reactivestreams.FlowAdapters

import reactor.adapter.JdkFlowAdapter
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import reactor.core.publisher.SynchronousSink

import scala.jdk.CollectionConverters.given
import scala.jdk.FunctionConverters.given

import urwerk.source.BackPressureStrategy
import urwerk.source.Source
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Optional
import urwerk.source.Singleton
import urwerk.source.Signal
import urwerk.source.Sink
import urwerk.source.SourceFactory
import urwerk.source.internal.given

private[source] object FluxSource extends SourceFactory:
  def apply[A](elems: A*): Source[A] = wrap(Flux.just(elems:_*))

  def create[A](op: Sink[A] => Unit): Source[A] =
    wrap(
      Flux.create[A](sink => op(FluxSink(sink))))

  def create[A](backpressure: BackPressureStrategy)(op: Sink[A] => Unit): Source[A] = 
    wrap(
      Flux.create[A](sink => op(FluxSink(sink)), 
        backpressure.asJava))

  def defer[A](op: => Source[A]): Source[A] =
    wrap(Flux.defer(() =>
      op.toFlux))

  def deferError[A](op: => Throwable): Source[A] =
    wrap(Flux.error(() => op))

  def empty[A]: Source[A] = wrap(Flux.empty)

  def error[A](error: Throwable): Source[A] = wrap(Flux.error(error))

  def from[A](publisher: Flow.Publisher[A]): Source[A] =
    wrap(
      JdkFlowAdapter.flowPublisherToFlux(publisher))

  def from[A](iterable: Iterable[A]): Source[A] =
    wrap(
      Flux.fromIterable(iterable.asJava))

  def push[A](op: Sink[A] => Unit): Source[A] =
    wrap(
      Flux.push[A](sink => op(FluxSink(sink))))

  def unfold[A, S](init: => S)(op: S => Option[(A, S)]): Source[A] =
    unfold(init, (_) => {})(op)

  def unfold[A, S](init: => S, doOnLastState: S => Unit)(op: S => Option[(A, S)]): Source[A] =
    val gen = (state: S, sink: SynchronousSink[A]) =>
      op(state) match {
        case Some((item, state)) =>
          sink.next(item)
          state
        case None =>
          sink.complete()
          state
      }

    val flux = Flux.generate(()=> init,
      gen.asJavaBiFunction,
      (state) => doOnLastState(state))

    wrap(flux)

  private[internal] def wrap[A](flux: Flux[A]): Source[A] = new FluxSource[A](flux)

private class FluxSource[+A](flux: Flux[_<: A]) extends FluxSourceOps[A](flux), Source[A]:
  import FluxSource.*

  type S[A] = Source[A]

  protected def wrap[B](flux: Flux[? <: B]): Source[B] = FluxSource.wrap(flux)

  def filter(pred: A => Boolean): Source[A] =
    wrap(flux.filter(pred(_)))

  def filterNot(pred: A => Boolean): Source[A] =
    filter(!pred(_))
