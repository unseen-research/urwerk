package urwerk.source

class SourceException(message: String, cause: Throwable) extends RuntimeException(message, cause):
  def this(cause: Throwable) = this("", cause)
  def this(message: String) = this("", null)