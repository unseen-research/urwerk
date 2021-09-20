package urwerk.source

trait Sink[A]:
  def complete(): Unit

  def error(error: Throwable): Unit

  def isCancelled: Boolean

  def next(item: A): Sink[A]

  def onCancel(op: => Unit): Sink[A]

  def onDispose(op: => Unit): Sink[A]

  def onRequest(op: Long => Unit): Sink[A]

  def requested: Long
