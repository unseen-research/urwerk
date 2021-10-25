package urwerk.app.command

import urwerk.app.App
import urwerk.io.ByteString
import urwerk.source.Source

object Commands

object TestApp extends App(Commands())

class Commands extends Function[Seq[String], Source[Either[ByteString, ByteString]]]:

  
  def apply(args: Seq[String]): Source[Either[ByteString, ByteString]] = Source()




