package urwerk.app

import scala.concurrent.Future
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.CommandCreation
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import urwerk.source.Signal

case class App(commands: Seq[CommandCreation]):
  final def main(args: Array[String]): Unit =

    Source.from(commands)
      .map(_.apply(unsafeWrapArray(args)))
      .flatMap(_.materialize)
      .takeUntil{case Signal.Next(_) => true
              case _ => false}
      .foldLeft(InitialScanResult){
        case (ctx, Signal.Next((out, err))) =>
          ctx.copy(output = out, errorOutput = err)
        case (ctx @ ScanResult(errors, _, _), Signal.Error(error)) =>
          ctx.copy(errors = errors :+ error)
        case (ctx, Signal.Complete) => ctx
      }
      .flatMap{case ScanResult(errors, out, err) =>
        if errors.nonEmpty then
          Source.error(errors(0))
        else
          Source(
              out
                .doOnNext(out => System.out.println(out))
                .filter(_ => false),
              err.doOnNext(out =>
                System.err.println(out)))
                .filter(_ => false)
            .concatDelayError
      }
      .lastOption
      .block

case class ScanResult(errors: Seq[Throwable], output: Source[String], errorOutput: Source[String])

val InitialScanResult = ScanResult(Seq(),
  Source.error(RuntimeException("missing command")),
  Source.error(RuntimeException("missing command")))
