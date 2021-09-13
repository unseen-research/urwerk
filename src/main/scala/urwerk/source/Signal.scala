package urwerk.source

import _root_.reactor.core.publisher.Signal as FluxSignal

enum Signal[+A]:
  case Error(error: Throwable) extends Signal[A]
  case Next(val next: A) extends Signal[A]
  case Complete extends Signal