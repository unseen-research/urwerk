package urwerk.source.internal

import java.io.IOException

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