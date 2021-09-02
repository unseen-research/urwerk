package urwerk.source

enum Signal[A]:
  case OnError extends Signal
  case OnNext(next: A) extends Signal[A]
  case OnComplete extends Signal