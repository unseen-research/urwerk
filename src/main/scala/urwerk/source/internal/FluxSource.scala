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
import reactor.core.publisher.SynchronousSink
import urwerk.source.Sink
import urwerk.source.SourceFactory

object FluxSource extends SourceFactory:

  def apply[A](elems: A*): Source[A] = 
    wrap(Flux.just(elems:_*))

  def create[A](op: Sink[A] => Unit): Source[A] = 
    wrap(  
      Flux.create[A](sink => op(FluxSink(sink))))

  def defer[A](op: => Source[A]): Source[A] =
    wrap(Flux.defer(() => 
      op.toFlux))

  def deferError[A](op: => Throwable): Source[A] =
    wrap(Flux.error(() => op))

  def error[A](error: Throwable): Source[A] =
    wrap(Flux.error(error))

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
    
  private def wrap[A](flux: Flux[A]): Source[A] = new FluxSource[A](flux)

  private def unwrap[A](source: Source[A]): Flux[A] =
    JdkFlowAdapter.flowPublisherToFlux(
      source.toPublisher.asInstanceOf[Flow.Publisher[A]])

class FluxSource[+A](val flux: Flux[_<: A]) extends Source[A]:
  import FluxSource.*

  def concat[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    wrap(
      Flux.concat(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux))) 

  def concatDelayError[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] =
    wrap(
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

  def doOnComplete(op: => Unit): Source[A] =
    wrap(
      flux.doOnComplete(() => op))
      
  def doOnError(op: Throwable => Unit): Source[A] =
    wrap(
      flux.doOnError(op(_)))
      
  def doOnNext(op: A => Unit): Source[A] =
    wrap(
      flux.doOnNext(op(_)))
    
  def filter(pred: A => Boolean): Source[A] =
    wrap(flux.filter(pred(_)))

  def filterNot(pred: A => Boolean): Source[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Source[B]): Source[B] = 
    wrap(
      flux.flatMap(
        op(_).toFlux))
    
  def flatMap[B](concurrency: Int)(op: A => Source[B]): Source[B] = 
    wrap(
      flux.flatMap(op(_).toFlux, 
      concurrency))

  def flatMap[B](concurrency: Int, prefetch: Int)(op: A => Source[B]): Source[B] = 
    wrap(
      flux.flatMap(op(_).toFlux,
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
    wrap(flux.map(op(_)))

  def materialize: Source[Signal[A]] = 
    wrap(
      flux.materialize.map(signal => FluxSignal(signal)))
  
  def merge[B >: A](that: Source[B]): Source[B] =
    wrap(
      Flux.merge(flux, unwrap(that)))

  def merge[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] = 
    wrap(
      Flux.merge(
        flux.asInstanceOf[Flux[Source[B]]]
          .map(_.toFlux)))

  def mergeDelayError[B >: A](prefetch: Int, that: Source[B]): Source[B] =
    wrap(
      Flux.mergeDelayError(prefetch, flux, unwrap(that)))

  def mkString(start: String, sep: String, end: String): Singleton[String] =
    fold(StringBuilder(start))((builder, elem) =>
        builder.append(elem.toString)
          .append(sep))
      .map(_.dropRight(sep.size)
        .append(end)
        .toString)

  def onErrorContinue(op: (Throwable, Any) => Unit): Source[A] = 
    wrap(
      flux.onErrorContinue(op.asJava))

  def onErrorResume[B >: A](op: Throwable => Source[B]): Source[B] = 
    wrap(
      flux.asInstanceOf[Flux[B]]
        .onErrorResume{(e) => op(e).toFlux})

  def reduce[B >: A](op: (B, A) => B): Optional[B] =
    def reduceOp[B1 <: A]: BiFunction[B1, B1, B1] = (v1, v2) =>
      op(v1, v2).asInstanceOf[B1]
    
    Optional.wrap(flux.reduce(reduceOp).flux)

  def scan[B](start: B)(op: (B, A) => B): Source[B] = 
    wrap(
      flux.scan(start, op.asJavaBiFunction))

  def scanWith[B](start: => B)(op: (B, A) => B): Source[B] = 
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

  def takeUntil(predicate: A => Boolean): Source[A] = 
    wrap(
      flux.takeUntil(predicate.asJava))

  def takeWhile(predicate: A => Boolean): Source[A] = 
    wrap(
      flux.takeWhile(predicate.asJava))
    
  def toPublisher[B >: A]: Flow.Publisher[B] =
    JdkFlowAdapter.publisherToFlowPublisher(flux.asInstanceOf[Flux[B]])

  def toSeq: Singleton[Seq[A]] =
    Singleton.wrap(flux
      .collectList
      .flux.map(_.asScala.toSeq))
