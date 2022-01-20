package urwerk.cli

import urwerk.test.TestBase

class CommandTest extends TestBase:
  
  object Opt:
    type TL = [X] =>> Map[X, String]

    val tl: TL[Int] = Map[Int, String]()

    class Wrapper[T]: 
      type X = T
      def get: X = ???


    def apply[A]: Opt[A, ?] = new Opt()


    def create[A](using w: Opt.Wrapper[?]): Opt[A, w.X] = ???

  class Opt[+A, +B]():

    def step1(): Opt[A, B] = this
    def step2(): Opt[A, B] = this

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

    Cmd[Config].option(Opt[String].configure((a, b)=>b))

    Cmd[Config].options(Opt[String].configure((a, b)=>b), Opt[Int].configure((a, b)=>b))
  }
