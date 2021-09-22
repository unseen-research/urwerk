package urwerk.source

trait Disposable:
  def dispose(): Unit
  def isDisposed(): Boolean
