package urwerk.source.internal

import java.io.IOException
import reactor.core.publisher.Flux
import urwerk.source.Source

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

private def wrap[A](flux: Flux[A]): Source[A] =
  new FluxSource[A](flux)