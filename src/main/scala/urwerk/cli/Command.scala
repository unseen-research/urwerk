package urwerk.cli

trait Command[C]():
  def parameterList(): Command[C] = ???

  def execute(op: C => Int): Int = ???


object Command: 

  trait Setting[V]: 
    def value: V

  trait Config[C]: 
      type C1 = C

      def value: C1


  type Action[C] = C => Int | Unit
  //def action(): Setting[]

  private def of[C]() = new Command[C]:

    val x = ""

  def apply[C](setting: Config[C] ?=> Setting[?], settings: Config[C] ?=> Setting[?]*): Command[C] = of()
  
  
  

