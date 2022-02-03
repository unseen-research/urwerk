package urwerk.cli

sealed class ParameterException(message: String, val position: Position) extends RuntimeException(message):
  def this(position: Position) = this("", position)
  def cause(exception: Throwable): Throwable = initCause(exception)

class IllegalValueException(position: Position) extends ParameterException(position)

class ParameterNotFoundException(position: Position, val param: Parameter[?, ?]) extends ParameterException(position)

class ValueNotFoundException(position: Position) extends ParameterException(position)

class ParameterValueRejected(position: Position) extends ParameterException(position)

class UnknownParameterException(position: Position) extends ParameterException(position)