package urwerk.cli

import urwerk.cli.Parameter.ConfigProvider
import urwerk.cli.Parameters.Position
import urwerk.cli.Parameters.ParameterList

case class Command[C](config: C, parameterLists: Seq[ParameterList[C]], applyOp: C => Int | Any):
  def apply(op: C => Int| Any): Command[C] = copy(applyOp = op)
    
  def execute(args: String*): Int = 

    val (_config, pos) = parameterLists.foldLeft((config, Position(0, 0))){case ((config, pos), paramList) =>
      paramList.collectParams(config, args, pos)
    }

    applyOp(_config) match
      case exitCode: Int => exitCode
      case _ => 0

  def parameterList(param: Parameter.ConfigProvider[C] ?=> Parameter[?, C], params: Parameter.ConfigProvider[C] ?=> Parameter[?, C]*): Command[C] = 
    val configProvider =  Parameter.configOf(config)

    val resolvedParam = param(using configProvider)
    val resolvedParams = params.map(param => param(using configProvider))
    val paramList = ParameterList(resolvedParam +: resolvedParams)
    
    copy(parameterLists = parameterLists :+ paramList)

object Command: 

  // trait Setting[V]: 
  //   def value: V

  trait Config[A]: 
      type B = A

      def value: B

  def apply[C](config: C): Command[C] = new Command(config, Seq(), _ => 1)
