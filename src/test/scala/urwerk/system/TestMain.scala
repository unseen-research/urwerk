package urwerk.system

object TestMain:
  def main(args: Array[String]): Unit =
    println(s"Greetings ${args.mkString}")
    sys.exit(77)
