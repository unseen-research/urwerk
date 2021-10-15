package urwerk.source.internal

import java.util.concurrent.Flow
import java.util.function.{BiConsumer, BiFunction}

import org.reactivestreams.FlowAdapters

import reactor.adapter.JdkFlowAdapter
import reactor.core.publisher.Flux
import reactor.core.publisher.FluxCreate
import reactor.core.publisher.Mono
import reactor.core.scheduler.Schedulers

import scala.concurrent.ExecutionContext
import scala.jdk.FunctionConverters.*
import scala.jdk.CollectionConverters.*

import urwerk.concurrent.given
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Optional
import urwerk.source.Singleton
import urwerk.source.Source
import urwerk.source.Signal
import urwerk.source.internal.given
import urwerk.source.BufferOverflowStrategy
import org.reactivestreams.Publisher

private abstract class FluxSourceOps[+A](val flux: Flux[_ <: A]):
  type  S[+ _]

  protected def wrap[B](flux: Flux[? <: B]): S[B]

  def cache: S[A] = wrap(flux.cache())

  def concat[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    FluxSource.wrap(
      Flux.concat(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux)))

  def concatDelayError[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    FluxSource.wrap(
      Flux.concatDelayError(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux)))

  def concat[A1 >: A](other: Source[A1]): Source[A1] =
    FluxSource.wrap(
      flux.asInstanceOf[Flux[A1]]
        .concatWith(other.toFlux))

  // def dematerialize[B](implicit evidence: Source[A] <:< Source[Signal[B]]): Source[B] =
  //   takeWhile{
  //     case Signal.Next(value) => true
  //     case Signal.Complete => false
  //     case Signal.Error(ex) =>
  //       throw ex
  //   }
  //   .map{_.asInstanceOf[Signal.Next[B]].value}

  def distinct: S[A] = wrap(flux.distinct)

  def doOnComplete(op: => Unit): S[A] =
    wrap(flux.doOnComplete(() => op))

  def doOnError(op: Throwable => Unit): S[A] =
    wrap(
      flux.doOnError(op(_)))

  def doOnNext(op: A => Unit): S[A] =
    wrap(
      flux.doOnNext(op(_)))

  def flatMap[B](op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(
        op(_).toFlux))

  def flatMap[B](op: A => S[B]): S[B] =
    wrap(
      flux.flatMap(elem =>
        unwrap(op(elem).asInstanceOf[Source[B]])))

  def flatMap[B](concurrency: Int)(op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(op(_).toFlux,
      concurrency))

  def flatMap[B](concurrency: Int, prefetch: Int)(op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(op(_).toFlux,
      concurrency,
      prefetch))

  def foldLeft[B](start: B)(op: (B, A) => B): urwerk.source.Singleton[B] =
    FluxSingleton.wrap(
      flux.reduce(start,
        op(_, _)).flux)

  def head: urwerk.source.Singleton[A] =
    FluxSingleton.wrap(
      flux
        .next()
        .single().flux())

  def headOption: Optional[A] =
    FluxOptional.wrap(
      flux
        .next().flux())

  def last: Singleton[A] =
    FluxSingleton.wrap(
      flux
        .last().flux())

  def lastOption: Optional[A] =
    FluxOptional.wrap(
      flux
        .last()
        .onErrorResume(
          classOf[NoSuchElementException],
          _ => Mono.empty)
        .flux())

  def map[B](op: A => B): S[B] =
    wrap(flux.map(op(_)))

  def materialize: S[Signal[A]] =
    wrap(
      flux.materialize.map(signal => FluxSignal(signal)))

  def merge[A1 >: A](that: Source[A1]): Source[A1] =
    FluxSource.wrap((
      Flux.merge(flux, unwrap(that))))

  def merge[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    FluxSource.wrap(
      Flux.merge(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux)))

  def mergeDelayError[A1 >: A](prefetch: Int, that: Source[A1]): Source[A1] =
    FluxSource.wrap(
      Flux.mergeDelayError(prefetch, flux, unwrap(that)))

  def mkString(start: String, sep: String, end: String): Singleton[String] =
    foldLeft(StringBuilder(start))((builder, elem) =>
        builder.append(elem.toString)
          .append(sep))
      .map(_.dropRight(sep.size)
        .append(end)
        .toString)

  def onBackpressureBuffer(capacity: Int, overflowStrategy: BufferOverflowStrategy): S[A] =
    wrap(
      flux.onBackpressureBuffer(capacity, overflowStrategy.asJava))

  def onErrorContinue(op: (Throwable, Any) => Unit): Source[A] =
    FluxSource.wrap(
      flux.onErrorContinue(op.asJava))

  def onErrorMap(op: Throwable => Throwable): S[A] =
    wrap(
      flux.onErrorMap(op.asJava))

  def onErrorResume[A1 >: A](op: Throwable => Source[A1]): Source[A1] =
    FluxSource.wrap(
      flux.asInstanceOf[Flux[A1]]
        .onErrorResume{(e) => op(e).toFlux})

  def onErrorResume[A1 >: A](op: Throwable => S[A1]): S[A1] =
    wrap(
      flux.asInstanceOf[Flux[A1]]
              .onErrorResume{(e) => unwrap(op(e).asInstanceOf[Source[A1]])})

  def reduce[A1 >: A](op: (A1, A) => A1): Optional[A1] =
    def reduceOp[B1 <: A]: BiFunction[B1, B1, B1] = (v1, v2) =>
      op(v1, v2).asInstanceOf[B1]

    FluxOptional.wrap(flux.reduce(reduceOp).flux)

  def scan[B](start: B)(op: (B, A) => B): S[B] =
    wrap(
      flux.scan(start, op.asJavaBiFunction))

  def scanWith[B](start: => B)(op: (B, A) => B): S[B] =
    wrap(
      flux.scanWith(()=> start, op.asJavaBiFunction))

  def subscribe(): AutoCloseable = {
    val disposable = flux.subscribe()
    () => {
      disposable.dispose()
    }
  }

  def subscribe[A1 >: A](subscriber: Flow.Subscriber[A1]): Unit = {
    flux.subscribe(
      FlowAdapters.toSubscriber(subscriber))
  }

  def subscribe(onNext: A => Unit, onError: Throwable => Unit, onComplete: => Unit ): AutoCloseable = {
    val disposable = flux.subscribe(onNext(_), onError(_), () => onComplete)
    () => {
      disposable.dispose()
    }
  }

  def subscribeOn(ec: ExecutionContext): S[A] =
    wrap(
      flux.subscribeOn(Schedulers.fromExecutor(ec.toExecutor)))

  def subscribeOn(ec: ExecutionContext, requestOnSeparateThread: Boolean): S[A] =
    wrap(
      flux.subscribeOn(Schedulers.fromExecutor(ec.toExecutor), requestOnSeparateThread))


  def takeUntil(predicate: A => Boolean): S[A] =
    wrap(
      flux.takeUntil(predicate.asJava))

  def takeWhile(predicate: A => Boolean): S[A] =
    wrap(
      flux.takeWhile(predicate.asJava))

  def toPublisher[A1 >: A]: Flow.Publisher[A1] =
    JdkFlowAdapter.publisherToFlowPublisher(flux.asInstanceOf[Flux[A1]])

  def toSeq: Singleton[Seq[A]] =
    FluxSingleton.wrap(flux
      .collectList
      .flux.map(_.asScala.toSeq))