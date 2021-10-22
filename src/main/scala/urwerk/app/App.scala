package urwerk.app

import scala.collection.immutable.ArraySeq

import urwerk.source.Optional
import urwerk.source.Source
import urwerk.io.ByteString
import urwerk.io.Streams.*
import urwerk.app.App.ExitStatus


type Main = Seq[String] => Source[Either[ByteString, ByteString]]

object App:
  object ExitStatus:
    type WithExitStatus = {
      def exitStatus: Int
    }

    def unapply(error: Throwable): Option[Int] = error match
      case es: ExitStatus =>
        Some(es.exitStatus)
      case _ => None

  trait ExitStatus(val exitStatus: Int)

class App(mainFn: Main):
  def main(args: Array[String]): Unit =
    val src = mainFn(ArraySeq.unsafeWrapArray(args))
      .doOnNext{
        case Left(out) =>
          System.out.write(out)
        case Right(out)=>
          System.out.write(out)
      }
      .lastOption

    try
      src.block
    catch
      case error@ExitStatus(status) =>
        sys.exit(status)
      case error: Throwable =>
        sys.exit(1)


