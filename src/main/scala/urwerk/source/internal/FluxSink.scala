package urwerk.source.internal

import reactor.core.publisher.{FluxSink => ReactorSink}
import urwerk.source.Subscriber

private class FluxSink[A](sink: ReactorSink[A]) extends Subscriber[A]:
  def complete(): Unit = sink.complete()

  def error(error: Throwable): Unit = sink.error(error)

  def isCancelled: Boolean = sink.isCancelled

  def next(elem: A): Subscriber[A] = FluxSink(sink.next(elem))

  def onCancel(op: => Unit): Subscriber[A] = FluxSink(sink.onCancel(() => op))

  def onDispose(op: => Unit): Subscriber[A] = FluxSink(sink.onDispose(() => op))

  def onRequest(op: Long => Unit): Subscriber[A] = FluxSink(sink.onRequest(op(_)))

  def requested: Long =
    sink.requestedFromDownstream()
