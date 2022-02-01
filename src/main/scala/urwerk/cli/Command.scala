package urwerk.cli

object Command: 
  
  sealed trait Setting
  
  case class ParameterListSetting[C](paramList: ParameterList[C]) extends Setting

  object Description:
    def := (description: String): DescriptionSetting = ???

  case class DescriptionSetting(description: String) extends Setting

  //def description: DescriptionSetting.type = ???

  // def paramList[C](using WithConfig[C])(name: String): ParameterListSetting.type = 
  //   ???

  def apply[C](param: WithConfig[C] ?=> Setting, params: WithConfig[C] ?=> Setting*): Command[C] = 
    ???

class Command[C](val paramLists: Seq[ParameterList[C]]):
  import Command.*