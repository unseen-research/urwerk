package urwerk.source

enum Signal[+A]:
  case Complete extends Signal
  case Error(error: Throwable) extends Signal[A]
  case Next(val next: A) extends Signal[A]
