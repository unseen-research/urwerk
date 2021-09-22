package urwerk.system

object TestMain:
  def main(args: Array[String]): Unit =

    val exitCode = args.to(Seq).applyOrElse(0, _ => "0").toInt
    val stdOutLine = args.to(Seq).applyOrElse(1, _ => "")
    val stdOutLineRepetition = args.to(Seq).applyOrElse(2, _ => "0").toInt

    val errOutLine = args.to(Seq).applyOrElse(3, _ => "")
    val errOutLineRepetition = args.to(Seq).applyOrElse(4, _ => "0").toInt

    for(i <- 0 to stdOutLineRepetition)
      println(stdOutLine)

    for(i <- 0 to errOutLineRepetition)
      println(errOutLine)

    sys.exit(exitCode)
