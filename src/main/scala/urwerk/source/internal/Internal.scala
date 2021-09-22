package urwerk.source.internal

import java.io.IOException

import reactor.core.publisher.Flux
import reactor.core.publisher.BufferOverflowStrategy as InnerBufferOverflowStrategy
import reactor.core.publisher.FluxSink.OverflowStrategy
import reactor.core.Disposable as ReactorDisposable

import urwerk.source.BackPressureStrategy
import urwerk.source.BufferOverflowStrategy

import urwerk.source.Disposable as Disposable
import urwerk.source.Source
import urwerk.source.reactor.FluxConverters.*

private def stripReactiveException[A](op: => A): A =
  try
    op
  catch
    case e: RuntimeException if e.getClass.getSimpleName() == "ReactiveException" =>
      val cause = e.getCause()
      if cause != null && cause.isInstanceOf[IOException] then
        throw cause
      else
        throw e
    case e: Throwable =>
      throw e

private def unwrap[B](source: Source[B]): Flux[B] = source.toFlux


extension(overflowStrategy: BufferOverflowStrategy)
  def asJava: InnerBufferOverflowStrategy =
    import urwerk.source.BufferOverflowStrategy.*
    overflowStrategy match
      case DropLatest => InnerBufferOverflowStrategy.DROP_LATEST
      case DropOldest => InnerBufferOverflowStrategy.DROP_OLDEST
      case Error =>  InnerBufferOverflowStrategy.ERROR

extension(backpressure: BackPressureStrategy)
  def asJava: OverflowStrategy =
    import urwerk.source.BackPressureStrategy.*
    backpressure match
      case Buffer => OverflowStrategy.BUFFER
      case Drop => OverflowStrategy.DROP
      case Error => OverflowStrategy.ERROR
      case Ignore => OverflowStrategy.IGNORE
      case Latest => OverflowStrategy.LATEST

object DisposableWrapper:
  def wrap(disposable: ReactorDisposable): Disposable =
    DisposableWrapper(disposable)

  def unwrap(disposable: Disposable): ReactorDisposable =
    if disposable.isInstanceOf[DisposableWrapper] then
      disposable.asInstanceOf[DisposableWrapper].inner
    else ???

class DisposableWrapper(val inner: ReactorDisposable) extends Disposable:
  export inner.*
