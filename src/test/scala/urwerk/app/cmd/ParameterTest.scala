package urwerk.app.cmd

import urwerk.test.TestBase

class ParameterTest extends TestBase:

  import Parameter.*

  "type" in {
    //class EitherMonad[T] extends Monad[[E] =>> Either[T, E]]
    //def apply[A](): Parameter[A] = new Parameter(None, (x: A) => ???)

    // trait P[A, B]

    // type C
    // def create[C]: P[C, ?] = ???


  //   def a(x: Int): String = ""

  //   Int => String

  //   type T[X] = R

  // type T = [X] =>> R


  }

  "default value" in {
    val param = Parameter[String]().default("some value")

    param.default.get should be("some value")
  }

  "on apply" in {
    case class Config(value: String)
    given ConfigProvider[Config] = () => Config("")

    Parameter[String]().onApply((config: Config, value) => config.copy(value = "test" + value)).apply("any value")
  }