package urwerk.app.command

import urwerk.app.command.Parameters.ParameterList
import urwerk.app.command.Parameters.Position
import urwerk.app.command.Parameters.UnexpectedParameterException
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.Parameters.ParameterException

object Command:
  def apply[A](name: String, config: A): Command[A] = Command[A](name, config, Seq(), _ => (Source(), Source()), "")

  extension [A](command: Command[A])
    def withArgs(args: Seq[String]): Optional[(Source[String], Source[String])] =
      _collectParams(command, args)

case class Command[A](name: String,
    config: A,
    parameterLists: Seq[ParameterList[A]],
    applyOp: A => (Source[String], Source[String]),
    description: String):

  def params(param: Parameter[?, A], params: Parameter[?, A]*): Command[A] =
    copy(parameterLists = parameterLists :+ ParameterList(param +: params))

  def apply(op: A => (Source[String], Source[String])): Command[A] =
    copy(applyOp = op)

private def _collectParams[A](command: Command[A], args: Seq[String]): Optional[(Source[String], Source[String])] =
  _collectParams(command.parameterLists, command.config, args, Position(0, 0)) match
    case Success(config) =>
      Optional(command.applyOp(config))
    case Failure(error: ParameterException) if error.position == Position(0, 0) =>
      Optional()
    case Failure(error: Throwable) =>
      Optional.error(error)

@tailrec
private def _collectParams[A](paramLists: Seq[ParameterList[A]], config: A, args: Seq[String], pos: Position): Try[A] =
  if paramLists.isEmpty then
    if pos.index < args.size then
      throw UnexpectedParameterException(pos)
    Success(config)
  else
    val paramList = paramLists.head
    val res = Try(
        paramList.collectParams(config, args, pos))

    res match
      case Success(_config, _pos) =>
        _collectParams(paramLists.drop(1), _config, args, _pos)
      case f: Failure[?] =>
        f.asInstanceOf[Failure[A]]
