package urwerk.main

import jdk.jshell.spi.ExecutionControl.NotImplementedException
import urwerk.cli.*
import urwerk.source.Source
import urwerk.source.Source.Sink

trait CommandSource:
  


// trait Command:
//   def apply(args: Seq[String]): Source[Output]


// class CommandFactory(commands: Seq[Command]) extends Function1[Seq[String], Source[Output]]:

//   def apply(args: Seq[String]): Source[Output] = Source.push {sink =>
//     try
//       command(args, sink)
//     catch
//       case error: Throwable =>
//         sink.error(error)
//   }
  
//   def command(args: Seq[String], sink: Sink[Output]): Unit =
//     val (options, leftArgs) = args.extractOptions(optionsSpecs*)
//     val (cmd, cmdArgs) = nextValue(leftArgs)
  
//     if isHelp(options) then
//       sink.next(StdOut(help))
//       sink.complete()
//     else if versionRequested(options) then
//       sink.next(StdOut(version))
//       sink.complete()
//     else
//       Source.error(NotImplementedException(""))

// private val optionsSpecs = Seq(
//   helpOption,
//   versionOption)
