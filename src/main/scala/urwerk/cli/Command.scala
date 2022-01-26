package urwerk.cli

trait Command[C]():
  def apply(op: C => Int| Any): Command[C]
    
  def execute(): Int

  def parameterList(): Command[C]
 

object Command: 

  trait Setting[V]: 
    def value: V

  trait Config[C]: 
      type C1 = C

      def value: C1

  def apply[C](config: C): Command[C] = of(config, _ => 1)

  //type Action[C] = C => Int | Unit
  //def action(): Setting[]

  private def of[C](config: C, applyOp: C => Int | Any): Command[C] = new Command[C]:
    def apply(op: C => Int | Any): Command[C] = copy(applyOp = op)
    
    def execute(): Int = 
      applyOp(config) match
        case exitCode: Int => exitCode
        case _ => 0

    def parameterList(): Command[C] = ???
    
    private def copy(config: C = config, applyOp: C => Int | Any = applyOp) = of(config, applyOp)