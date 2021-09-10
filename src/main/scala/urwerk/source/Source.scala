package urwerk.source

import _root_.reactor.adapter.JdkFlowAdapter
import _root_.reactor.core.publisher.{Flux, FluxSink, Mono, SynchronousSink}
import org.reactivestreams.FlowAdapters
import urwerk.source.reactor.FluxConverters.*

import java.util.concurrent.Flow
import java.util.function.{BiConsumer, BiFunction}
import scala.collection.mutable.StringBuilder
import scala.jdk.CollectionConverters.given
import scala.jdk.FunctionConverters.given
import scala.util.Try
import urwerk.source.internal.FluxSource

object Source:

  def apply[A](elems: A*): Source[A] = 
    wrap(Flux.just(elems:_*))

  def create[A](op: Sink[A] => Unit): Source[A] = 
    wrap(  
      Flux.create[A](sink => op(wrapSink(sink))))

  def defer[A](op: => Source[A]): Source[A] =
    wrap(Flux.defer(() => 
      op.asFlux))

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
      Flux.push[A](sink => op(wrapSink(sink))))

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
  
  private[source] def wrap[A](flux: Flux[A]): Source[A] = FluxSource[A](flux)

  private[source] def unwrap[A](source: Source[A]): Flux[A] =
    JdkFlowAdapter.flowPublisherToFlux(
      source.toPublisher.asInstanceOf[Flow.Publisher[A]])    
      
  private[source] def wrapSink[A](sink: FluxSink[A]): Sink[A] = 
    new Sink[A]: 
      def complete(): Unit = 
        sink.complete()

      def error(error: Throwable): Unit =
        sink.error(error)
  
      def isCancelled: Boolean =
        sink.isCancelled
  
      def next(elem: A): Sink[A] =
        wrapSink(sink.next(elem))
  
      def onCancel(op: => Unit): Sink[A] =
        wrapSink(sink.onCancel(() => op))
  
      def onDispose(op: => Unit): Sink[A] =
        wrapSink(sink.onDispose(() => op))
  
      def onRequest(op: Long => Unit): Sink[A] =
        wrapSink(sink.onRequest(op(_)))
  
      def requested: Long = 
        sink.requestedFromDownstream()


end Source

trait Source[+A]:

  def concat[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] 

  def concatDelayError[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B] 

  //def dematerialize[B](implicit evidence: Source[A] <:< Source[Signal[B]]): Source[B]

  def doOnComplete(op: => Unit): Source[A]
  
  def doOnError(op: Throwable => Unit): Source[A]
  
  def doOnNext(op: A => Unit): Source[A]
  
  def filter(pred: A => Boolean): Source[A]

  def filterNot(pred: A => Boolean): Source[A]

  def flatMap[B](op: A => Source[B]): Source[B]

  def flatMap[B](concurrency: Int)(op: A => Source[B]): Source[B]

  def flatMap[B](concurrency: Int, prefetch: Int)(op: A => Source[B]): Source[B]

  def fold[B](start: B)(op: (B, A) => B): Singleton[B]

  def head: Singleton[A]

  def headOption: Optional[A]

  def last: Singleton[A]

  def lastOption: Optional[A]
  
  def map[B](op: A => B): Source[B]

  def materialize: Source[Signal[A]]

  def merge[B >: A](that: Source[B]): Source[B]

  def merge[B](implicit evidence: Source[A] <:< Source[Source[B]]): Source[B]

  def mergeDelayError[B >: A](prefetch: Int, that: Source[B]): Source[B]

  @inline final def mkString: Singleton[String] = mkString("")

  @inline final def mkString(sep: String): Singleton[String] = mkString("", sep, "")

  def mkString(start: String, sep: String, end: String): Singleton[String]

  def onErrorContinue(op: (Throwable, Any) => Unit): Source[A]

  def onErrorResume[B >: A](op: Throwable => Source[B]): Source[B]
  
  def reduce[B >: A](op: (B, A) => B): Optional[B]

  def scan[B](start: B)(op: (B, A) => B): Source[B]
  
  def scanWith[B](start: => B)(op: (B, A) => B): Source[B]

  def subscribe(): AutoCloseable

  def subscribe[B >: A](subscriber: Flow.Subscriber[B]): Unit

  def subscribe(onNext: A => Unit, onError: Throwable => Unit, onComplete: => Unit ): AutoCloseable

  def takeUntil(predicate: A => Boolean): Source[A]

  def takeWhile(predicate: A => Boolean): Source[A]

  def toPublisher[B >: A]: Flow.Publisher[B]

  def toSeq: Singleton[Seq[A]]

end Source