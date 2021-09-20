package urwerk.source.internal

import java.io.IOException

import reactor.core.publisher.Flux
import reactor.core.publisher.{BufferOverflowStrategy => FluxBufferOverflowStrategy}

import urwerk.source.BufferOverflowStrategy
import urwerk.source.BufferOverflowStrategy.*
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
  def asJava: FluxBufferOverflowStrategy =
    overflowStrategy match
      case DropLatest => FluxBufferOverflowStrategy.DROP_LATEST
      case DropOldest => FluxBufferOverflowStrategy.DROP_OLDEST
      case Error =>  FluxBufferOverflowStrategy.ERROR
