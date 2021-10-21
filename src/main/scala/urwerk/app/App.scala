package urwerk.app

import scala.util.control.NonFatal

import urwerk.source.Optional
import urwerk.source.Source
import urwerk.io.ByteString
import urwerk.io.Streams.*

type Main = Source[ByteString] => Source[Either[ByteString, ByteString]]

trait App(mainFn: Main):
  def main(args: Array[String]): Unit =
    val src = mainFn(Source())
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
      case error: Throwable =>
        println("ERROR")

