package urwerk.source

import _root_.reactor.core.publisher.Signal as FluxSignal

object Signal:
  def apply[A](signal: FluxSignal[A]): Signal[A] = 
    if signal.isOnError() then
      Error(signal.getThrowable())
    else if signal.isOnNext() then
      Next(signal.get)
    else if signal.isOnComplete() then
      Complete
    else throw UnsupportedOperationException()

enum Signal[+A]:
  case Error(error: Throwable) extends Signal[A]
  case Next(val next: A) extends Signal[A]
  case Complete extends Signal