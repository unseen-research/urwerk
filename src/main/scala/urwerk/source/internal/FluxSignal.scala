package urwerk.source.internal

import reactor.core.publisher.Signal as ReactorSignal
import urwerk.source.Signal
import urwerk.source.Signal.{Next, Complete, Error}

object FluxSignal:
  def apply[A](signal: ReactorSignal[A]): Signal[A] =
    if signal.isOnError() then
        Error(signal.getThrowable())
    else if signal.isOnNext() then
        Next(signal.get)
    else if signal.isOnComplete() then
        Complete
    else throw UnsupportedOperationException()