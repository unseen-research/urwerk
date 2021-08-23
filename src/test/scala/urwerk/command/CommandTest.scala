package urwerk.command

import urwerk.test.TestBase

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "st" in {
    import scala.compiletime.{constValue}

    inline def toIntC[N <: Int] : Int =
      constValue[N]

    class Abc[A]
    class ToInt[N<: Int]:
      inline def int(): Int = constValue[N]

    val ctwo = toIntC[2]

    println(s"xxx $ctwo")

  }

  "test" in {
    val cmd = Command("command")
      .params(
        param[String]("name1"),
        param[Int]("name2"))
      // .params(
      //   param["String"]
      // )
      .params(
        param[String]
          .arity(0, 77)
          .accept(_ => true)
      )

    cmd(Seq("--name2", "55", "--name1", "value1", "string value"))
  }