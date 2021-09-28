package urwerk.system

object TestMain:
  def main(args: Array[String]): Unit =
    main(args.to(Seq))

  def main(args: Seq[String]): Unit =
    try
      val exitCode = args.applyOrElse(0, _ => "0").toInt
      val stdOutLine = args.applyOrElse(1, _ => "")
      val stdOutLineRepetition = args.applyOrElse(2, _ => "0").toInt

      val errOutLine = args.applyOrElse(3, _ => "")
      val errOutLineRepetition = args.applyOrElse(4, _ => "0").toInt

      val repetition = math.max(stdOutLineRepetition, errOutLineRepetition)

      for(i <- 0 until repetition)
        if i < stdOutLineRepetition then
          System.out.println(stdOutLine)

        if i < errOutLineRepetition then
          System.err.println(errOutLine)

      sys.exit(exitCode)
    catch
      case e: Throwable =>
        println(s"ERROR $e")
        sys.exit(-111)

