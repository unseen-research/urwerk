package urwerk.command

import urwerk.test.TestBase

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "test" in {
    val cmd = Command()
      .params(
        param[String]("name1"),
        param[Int]("name2"))
      .params(
        param[String]
      )
      .params(
        param[String]
          .arity(0, 77)
          .accept(_ => true)
      )

    cmd.
  }