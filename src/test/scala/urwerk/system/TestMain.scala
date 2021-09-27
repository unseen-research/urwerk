package urwerk.system

object TestMain:
  def main(args: Array[String]): Unit =
    main(args.to(Seq))

  def main(args: Seq[String]): Unit =
    val exitCode = args.applyOrElse(0, _ => "0").toInt
    val stdOutLine = args.applyOrElse(1, _ => "")
    val stdOutLineRepetition = args.applyOrElse(2, _ => "0").toInt

    val errOutLine = args.applyOrElse(3, _ => "")
    val errOutLineRepetition = args.applyOrElse(4, _ => "0").toInt

    val repetition = math.max(stdOutLineRepetition, errOutLineRepetition)

    println(s"ARGS $exitCode $stdOutLine $stdOutLineRepetition")
    for(i <- 0 to repetition)
      if i < stdOutLineRepetition then
        println("std " + stdOutLine)
        System.out.println(stdOutLine)

      if i < errOutLineRepetition then
        System.err.println(errOutLine)

    sys.exit(exitCode)