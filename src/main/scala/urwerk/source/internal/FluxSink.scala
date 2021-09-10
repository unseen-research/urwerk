package urwerk.source.internal

import reactor.core.publisher.{FluxSink => ReactorSink}
import urwerk.source.Sink

class FluxSink[A](sink: ReactorSink[A]) extends Sink[A]:
  def complete(): Unit = 
    sink.complete()

  def error(error: Throwable): Unit = sink.error(error)

  def isCancelled: Boolean = sink.isCancelled

  def next(elem: A): Sink[A] = FluxSink(sink.next(elem))

  def onCancel(op: => Unit): Sink[A] = FluxSink(sink.onCancel(() => op))

  def onDispose(op: => Unit): Sink[A] = FluxSink(sink.onDispose(() => op))

  def onRequest(op: Long => Unit): Sink[A] = FluxSink(sink.onRequest(op(_)))

  def requested: Long = 
    sink.requestedFromDownstream()
