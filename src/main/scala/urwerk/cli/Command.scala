package urwerk.cli

object Command: 
  
  sealed trait Setting

  class ParameterListFactory[C]():
    def := (param: Seq[Parameter[?, C]]): ParameterListSetting[C] = ???
  
  case class ParameterListSetting[C](paramList: ParameterList[C]) extends Setting

  class DescriptionSettingFactory():
    def := (description: String): DescriptionSetting = ???

  case class DescriptionSetting(description: String) extends Setting

  def description: DescriptionSettingFactory = ???

  def paramList[C](using ConfigEvidence[C])(name: String): ParameterListFactory[C] = 
    ???

  def apply[C](param: ConfigEvidence[C] ?=> Setting, params: ConfigEvidence[C] ?=> Setting*): Command[C] = 
    ???

class Command[C](val paramLists: Seq[ParameterList[C]]):
  import Command.*