package urwerk.main

import urwerk.source.Source
import urwerk.test.TestBase
import urwerk.source.Optional

class MainTest extends TestBase:
  "error while create command" in withOut{(out, err) =>
    var actualError: Throwable = RuntimeException()
    var actualExitStatus = 0

    def cmd(args: Seq[String]): Source[Either[String, String]] = 
      throw IllegalArgumentException()

    Main(cmd, error => actualError = error, exitStatus => actualExitStatus = exitStatus)
      .main(Array())

    actualError shouldBe a [IllegalArgumentException]
    actualExitStatus should be(-1)
  }


  "command completes with error" in {
    var actualError: Throwable = RuntimeException()
    var actualExitStatus = 0

    def cmd(args: Seq[String]): Source[Either[String, String]] = 
      Optional.error(IllegalStateException())

    Main(cmd, error => actualError = error, exitStatus => actualExitStatus = exitStatus)
      .main(Array())

    actualError shouldBe a [IllegalStateException]
    actualExitStatus should be(-1)
  }

  "command completes with error and exit status" in {
    var actualError: Throwable = RuntimeException()
    var actualExitStatus = 0

    def cmd(args: Seq[String]): Source[Either[String, String]] = 
      Optional.error(new IllegalStateException with ExitStatus {def exitStatus = - 7 })

    Main(cmd, error => actualError = error, exitStatus => actualExitStatus = exitStatus)
      .main(Array())

    actualError shouldBe a [IllegalStateException]
    actualExitStatus should be(-7)
  }

  "standard output" in withOut{(out, err) =>
    var actualExitStatus = 0

    def cmd(args: Seq[String]): Source[Either[String, String]] = 
      Source(Right("abc"), Right("def"), Right("ghi"))

    Main(cmd, error => (), exitStatus => actualExitStatus = exitStatus)
      .main(Array())

    out.toString should be ("abcdefghi")
    err.toString should be ("")
    actualExitStatus should be(0)
  }

  "error output" in withOut{(out, err) =>
    var actualExitStatus = 0

    def cmd(args: Seq[String]): Source[Either[String, String]] = 
      Source(Left("abc"), Left("def"), Left("ghi"))
    
    Main(cmd, error => (), exitStatus => actualExitStatus = exitStatus)
      .main(Array())

    err.toString should be ("abcdefghi")
    out.toString should be ("")
    
    actualExitStatus should be(0)
  }
