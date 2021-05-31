package urwerk.io.http

class HttpException(message: String) extends RuntimeException(message: String):
  def withCause(cause: Throwable): Throwable =
    initCause(cause)

class HttpStatusException(val statusCode: Int, message: String) extends HttpException(message):
  def this(statusCode: Int) = this(statusCode, s"Http request failed: statusCode=$statusCode")

enum Method(val name: String):
  case Get  extends Method("GET")
  case Head  extends Method("HEAD")
  case Post  extends Method("POST")
  case Put extends Method("PUT")
  case Delete extends Method("DELETE")
  case Connect extends Method("CONNECT")
  case Options extends Method("OPTIONS")
  case Trace extends Method("TRACE")
  case Other(method: String) extends Method(method)

