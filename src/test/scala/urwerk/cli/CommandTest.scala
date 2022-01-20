package urwerk.cli

import urwerk.test.TestBase

class CommandTest extends TestBase:
  
  object Opt:
    type TL = [X] =>> Opt[X, ?]


    class Wrapper[A]: 
      type X = TL[A]

      def get: X = ???


    def apply[A]: Opt[A, ?] = new Opt()


    def create[A]()(using w: Opt.Wrapper[A]): w.X = ???

  class Opt[A, B](val v: B):
    def this() = this(???)

    def step1[B1 >: B]: Opt[A, B1] = new Opt(v)
    def step2[B1 >: B]: Opt[A, B1] = new Opt(v)

    def configure[B1](fn: (A, B1) => B1): Opt[A, B1] = new Opt[A, B1]
     
    
  class Cmd[B]:
    def option(opts: Opt.Wrapper[B] ?=> Opt[?, B]) = ""

    def options(opts: Opt.Wrapper[B] ?=> Opt[?, B] *) = 
      val list = opts.map{ fn =>
          val w: Opt.Wrapper[B] = ???

          fn(using w)
      }

    
  "overlord" in {

    given w: Opt.Wrapper[Int]()

    val op: Opt[String, Int] = Opt.create[String]

  }

  "overload" in {

    class Config()

    Cmd[Config].option(Opt[String].create)

    Cmd[Config].options(Opt[String].configure((a, b)=>b), Opt[Int].configure((a, b)=>b))
  }
