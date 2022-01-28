package urwerk.cli

case class Command[C](config: C, parameterLists: Seq[ParameterList[C]], applyOp: C => Int | Any):
  def apply(op: C => Int| Any): Command[C] = copy(applyOp = op)
    
  def execute(args: String*): Int = 

    val (_config, pos) = parameterLists.foldLeft((config, Position(0, 0))){case ((config, pos), paramList) =>
      paramList.collect(config, args)
    }

    applyOp(_config) match
      case exitCode: Int => exitCode
      case _ => 0

  def parameterList(param: ConfigEvidence[C] ?=> Parameter[?, C], params: ConfigEvidence[C] ?=> Parameter[?, C]*): Command[C] = 
    val paramList = ParameterList(param, params*)
    copy(parameterLists = parameterLists :+ paramList)

object Command: 

  // trait Setting[V]: 
  //   def value: V

  trait Config[A]: 
      type B = A

      def value: B

  def apply[C](config: C): Command[C] = new Command(config, Seq(), _ => 1)
