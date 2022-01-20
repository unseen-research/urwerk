package urwerk.cli

import urwerk.test.TestBase

class CommandTest extends TestBase:
  
  object Opt:
    //type TL = [X] =>> Opt[X, ?]


    class Wrapper[A]: 
      type X = A

      def get: X = ???


    def apply[A](using w: Opt.Wrapper[?]): Opt[A, w.X] = ???


    def create[A](using w: Opt.Wrapper[?]): Opt[A, w.X] = ???

  class Opt[A, B](val v: B):
    def this() = this(???)

    def step1: Opt[A, B] = new Opt[A, B]()
    def step2: Opt[A, B] = ??? //new Opt(v)

    def configure(fn: (A, B) => B): Opt[A, B] = new Opt[A, B]
     
    
  class Cmd[B]:
    def option(opts: Opt.Wrapper[B] ?=> Opt[?, B]) = ""

    def options(opts: Opt.Wrapper[B] ?=> Opt[?, B] *) = 
      val list = opts.map{ fn =>
          val w: Opt.Wrapper[B] = ???

          fn(using w)
      }
   

  "overload" in {

    class Config()

    Cmd[Config].option(x ?=>
      Opt.create[String](using x))


    Cmd[Config].option(
      Opt.create[String])

    Cmd[Config].options(
      Opt.create[String].configure((a, b)=>b), 
      Opt.create[Int].step1.step2.configure((a: Int, b)=>b).step1.step2)
  }


  "apply" in {

    class Config()

    Cmd[Config].option(x ?=>
      Opt[String](using x))


    Cmd[Config].option(
      Opt[String])

    Cmd[Config].options(
      Opt[String].configure((a, b)=>b), 
      Opt[Int].step1.step2.configure((a: Int, b: Config)=>b).step1.step2)
  }
