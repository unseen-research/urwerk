package urwerk.source

trait Subscriber[A]:
  def complete(): Unit

  def error(error: Throwable): Unit

  def isCancelled: Boolean

  def next(item: A): Subscriber[A]

  def onCancel(op: => Unit): Subscriber[A]

  def onDispose(op: => Unit): Subscriber[A]

  def onRequest(op: Long => Unit): Subscriber[A]

  def requested: Long
