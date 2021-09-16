package urwerk.source

import java.util.concurrent.Flow

import urwerk.source.internal.FluxSource

object Source extends SourceFactory:
  export FluxSource.*

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

  def onErrorMap(op: Throwable => Throwable): Source[A]

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

trait SourceFactory:
  def apply[A](elems: A*): Source[A]

  def create[A](op: Subscriber[A] => Unit): Source[A]

  def defer[A](op: => Source[A]): Source[A]

  def deferError[A](op: => Throwable): Source[A]

  def empty[A]: Source[A]

  def error[A](error: Throwable): Source[A]

  def from[A](publisher: Flow.Publisher[A]): Source[A]

  def from[A](iterable: Iterable[A]): Source[A]

  def push[A](op: Subscriber[A] => Unit): Source[A]

  def unfold[A, S](init: => S)(op: S => Option[(A, S)]): Source[A]

  def unfold[A, S](init: => S, doOnLastState: S => Unit)(op: S => Option[(A, S)]): Source[A]
