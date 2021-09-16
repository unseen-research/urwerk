package urwerk.source.internal

import java.util.function.{BiConsumer, BiFunction}

import org.reactivestreams.FlowAdapters

import reactor.adapter.JdkFlowAdapter
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono

import scala.jdk.FunctionConverters.*
import scala.jdk.CollectionConverters.*

import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Optional
import urwerk.source.Singleton
import urwerk.source.Source
import urwerk.source.Signal
import java.util.concurrent.Flow

trait Aux[+S[_]]:
  def from[A](flux: Flux[? <: A]): S[A]

abstract class FluxSourceOps[+A, S[_]](val flux: Flux[_<: A]):

  protected def wrap[B](flux: Flux[? <: B]): S[B]

  protected def unwrap[B, S1[_] >: S[B]](src: S1[B]): Flux[B]

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

  // def dematerialize[B](implicit evidence: Source[A] <:< Source[Signal[B]]): Source[B] =
  //   takeWhile{
  //     case Signal.Next(value) => true
  //     case Signal.Complete => false
  //     case Signal.Error(ex) =>
  //       throw ex
  //   }
  //   .map{_.asInstanceOf[Signal.Next[B]].value}

  def doOnComplete[A1 >: A](op: => Unit): S[A1] =
    wrap(flux.doOnComplete(() => op))

  def doOnError[A1 >: A](op: Throwable => Unit): S[A1] =
    wrap(
      flux.doOnError(op(_)))

  def doOnNext[A1 >: A](op: A => Unit): S[A1] =
    wrap(
      flux.doOnNext(op(_)))

  // def filter(pred: A => Boolean): Source[A] =
  //   wrap(flux.filter(pred(_)))

  // def filterNot(pred: A => Boolean): Source[A] =
  //   filter(!pred(_))

  def flatMap[B](op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(
        op(_).toFlux))

  def flatMap[B](op: A => S[B]): S[B] =
    wrap(
      flux.flatMap(elem =>
        unwrap(op(elem))))

  def flatMap[B](concurrency: Int)(op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(op(_).toFlux,
      concurrency))

  def flatMap[B](concurrency: Int, prefetch: Int)(op: A => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.flatMap(op(_).toFlux,
      concurrency,
      prefetch))

  def fold[B](start: B)(op: (B, A) => B): urwerk.source.Singleton[B] =
    Singleton.wrap(
      flux.reduce(start,
        op(_, _)).flux)

  def head: urwerk.source.Singleton[A] =
    Singleton.wrap(
      flux
        .next()
        .single().flux())

  def headOption: Optional[A] =
    Optional.wrap(
      flux
        .next().flux())

  def last: Singleton[A] =
    Singleton.wrap(
      flux
        .last().flux())

  def lastOption: Optional[A] =
    Optional.wrap(
      flux
        .last()
        .onErrorResume(
          classOf[NoSuchElementException],
          _ => Mono.empty)
        .flux())

  def map[B](op: A => B): S[B] =
    wrap(flux.map(op(_)))

  def materialize[A1 >: A]: S[Signal[A1]] =
    wrap(
      flux.materialize.map(signal => FluxSignal(signal)))

  def merge[B >: A](that: Source[B]): Source[B] =
    FluxSource.wrap((
      Flux.merge(flux, FluxSource.unwrap(that))))

  def merge[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    FluxSource.wrap(
      Flux.merge(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux)))

  def mergeDelayError[B >: A](prefetch: Int, that: Source[B]): Source[B] =
    FluxSource.wrap(
      Flux.mergeDelayError(prefetch, flux, FluxSource.unwrap(that)))

  def mkString(start: String, sep: String, end: String): Singleton[String] =
    fold(StringBuilder(start))((builder, elem) =>
        builder.append(elem.toString)
          .append(sep))
      .map(_.dropRight(sep.size)
        .append(end)
        .toString)

  def onErrorContinue(op: (Throwable, Any) => Unit): Source[A] =
    FluxSource.wrap(
      flux.onErrorContinue(op.asJava))

  def onErrorMap[A1 >: A](op: Throwable => Throwable): S[A1] =
    wrap(
      flux.onErrorMap(op.asJava))

  def onErrorResume[B >: A](op: Throwable => Source[B]): Source[B] =
    FluxSource.wrap(
      flux.asInstanceOf[Flux[B]]
        .onErrorResume{(e) => op(e).toFlux})

  def onErrorResume[B >: A](op: Throwable => S[B]): S[B] =
    wrap(
      flux.asInstanceOf[Flux[B]]
        .onErrorResume{(e) => unwrap(op(e))})

  def reduce[B >: A](op: (B, A) => B): Optional[B] =
    def reduceOp[B1 <: A]: BiFunction[B1, B1, B1] = (v1, v2) =>
      op(v1, v2).asInstanceOf[B1]

    Optional.wrap(flux.reduce(reduceOp).flux)

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

  def subscribe[B >: A](subscriber: Flow.Subscriber[B]): Unit = {
    flux.subscribe(
      FlowAdapters.toSubscriber(subscriber))
  }

  def subscribe(onNext: A => Unit, onError: Throwable => Unit, onComplete: => Unit ): AutoCloseable = {
    val disposable = flux.subscribe(onNext(_), onError(_), () => onComplete)
    () => {
      disposable.dispose()
    }
  }

  def takeUntil[A1 >: A](predicate: A => Boolean): S[A1] =
    wrap(
      flux.takeUntil(predicate.asJava))

  def takeWhile[A1 >: A](predicate: A => Boolean): S[A1] =
    wrap(
      flux.takeWhile(predicate.asJava))

  def toPublisher[B >: A]: Flow.Publisher[B] =
    JdkFlowAdapter.publisherToFlowPublisher(flux.asInstanceOf[Flux[B]])

  def toSeq: Singleton[Seq[A]] =
    Singleton.wrap(flux
      .collectList
      .flux.map(_.asScala.toSeq))