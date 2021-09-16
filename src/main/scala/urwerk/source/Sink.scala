package urwerk.source

trait Sink[A]:
  def emitNext(next: A): Sink[A]

  def emitError(error: Throwable): Unit

  def emitComplete(): Unit

  def source: Source[A]

  def subscriberCount: Int