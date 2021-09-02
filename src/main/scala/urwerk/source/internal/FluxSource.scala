package urwerk.source.internal

import java.util.function.{BiConsumer, BiFunction}

import reactor.core.publisher.Flux
import urwerk.source.Source
import urwerk.source.reactor.FluxConverters.*
import urwerk.source.Optional
import urwerk.source.Singleton
import reactor.core.publisher.Mono

import scala.jdk.CollectionConverters.given
import scala.jdk.FunctionConverters.given
import org.reactivestreams.FlowAdapters
import java.util.concurrent.Flow
import reactor.adapter.JdkFlowAdapter
import urwerk.source.Signal

class FluxSource[+A](val flux: Flux[_<: A]) extends Source[A]:
  def concat[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    Source.wrap(
      Flux.concat(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.asFlux))) 

  def concatDelayError[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    Source.wrap(
      Flux.concatDelayError(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.asFlux)))

  def doOnComplete(op: => Unit): Source[A] =
    Source.wrap(
      flux.doOnComplete(() => op))
      
  def doOnError(op: Throwable => Unit): Source[A] =
    Source.wrap(
      flux.doOnError(op(_)))
      
  def doOnNext(op: A => Unit): Source[A] =
    Source.wrap(
      flux.doOnNext(op(_)))
    
  def filter(pred: A => Boolean): Source[A] =
    Source.wrap(flux.filter(pred(_)))

  def filterNot(pred: A => Boolean): Source[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Source[B]): Source[B] = 
    Source.wrap(
      flux.flatMap(
        op(_).asFlux))
    
  def flatMap[B](concurrency: Int)(op: A => Source[B]): Source[B] = 
    Source.wrap(
      flux.flatMap(op(_).asFlux, 
      concurrency))

  def flatMap[B](concurrency: Int, prefetch: Int)(op: A => Source[B]): Source[B] = 
    Source.wrap(
      flux.flatMap(op(_).asFlux,
      concurrency,
      prefetch))
  
  def fold[B](start: B)(op: (B, A) => B): urwerk.source.Singleton[B] =
    urwerk.source.Singleton.wrap(
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
        
  def map[B](op: A => B): Source[B] =
    Source.wrap(flux.map(op(_)))

  def materialize: Source[Signal[A]] = 
    Source.wrap(
      flux.materialize.map(signal => Signal(signal)))
  
  def merge[B >: A](that: Source[B]): Source[B] =
    Source.wrap(
      Flux.merge(flux, Source.unwrap(that)))

  def merge[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] = 
    Source.wrap(
      Flux.merge(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.asFlux)))

  def mergeDelayError[B >: A](prefetch: Int, that: Source[B]): Source[B] =
    Source.wrap(
      Flux.mergeDelayError(prefetch, flux, Source.unwrap(that)))

  def mkString(start: String, sep: String, end: String): Singleton[String] =
    fold(StringBuilder(start))((builder, elem) =>
        builder.append(elem.toString)
          .append(sep))
      .map(_.dropRight(sep.size)
        .append(end)
        .toString)

  def onErrorContinue(op: (Throwable, Any) => Unit): Source[A] = 
    Source.wrap(
      flux.onErrorContinue(op.asJava))

  def onErrorResume[B >: A](op: Throwable => Source[B]): Source[B] = 
    Source.wrap(
      flux.asInstanceOf[Flux[B]]
        .onErrorResume{(e) => op(e).asFlux})

  def reduce[B >: A](op: (B, A) => B): Optional[B] =
    def reduceOp[B1 <: A]: BiFunction[B1, B1, B1] = (v1, v2) =>
      op(v1, v2).asInstanceOf[B1]
    
    Optional.wrap(flux.reduce(reduceOp).flux)

  def scan[B](start: B)(op: (B, A) => B): Source[B] = 
    Source.wrap(
      flux.scan(start, op.asJavaBiFunction))

  def scanWith[B](start: => B)(op: (B, A) => B): Source[B] = 
    Source.wrap(
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

  def toPublisher[B >: A]: Flow.Publisher[B] =
    JdkFlowAdapter.publisherToFlowPublisher(flux.asInstanceOf[Flux[B]])

  def toSeq: Singleton[Seq[A]] =
    Singleton.wrap(flux
      .collectList
      .flux.map(_.asScala.toSeq))
