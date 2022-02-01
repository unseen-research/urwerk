package urwerk.cli

object Command: 
  
  sealed trait Setting

  object ParameterListSetting:
    def :=[C] (param: Seq[Parameter[?, C]]): ParameterListSetting[C] = ???
  
  case class ParameterListSetting[C](paramList: ParameterList[C]) extends Setting

  object DescriptionSetting:
    def := (description: String): DescriptionSetting = ???

  case class DescriptionSetting(description: String) extends Setting

  def description: DescriptionSetting.type = ???

  def paramList[C](using ConfigEvidence[C])(name: String): ParameterListSetting.type = 
    ???

  def apply[C](param: ConfigEvidence[C] ?=> Setting, params: ConfigEvidence[C] ?=> Setting*): Command[C] = 
    ???

class Command[C](val paramLists: Seq[ParameterList[C]]):
  import Command.*