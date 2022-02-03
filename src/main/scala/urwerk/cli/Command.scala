package urwerk.cli

object Command: 
  
  sealed trait Setting

  object Action:
    def :=[C](using ev: WithConfig[C])(action: C=>Int): ActionSetting[C] = ActionSetting(action)
    

  case class ActionSetting[C](action: C=>Int) extends Setting

  case class ParameterListSetting[C](paramList: ParameterList[C]) extends Setting

  object Description:
    def := (description: String): DescriptionSetting = DescriptionSetting(description)

  case class DescriptionSetting(description: String) extends Setting

  def apply[C](config: C)(setting: WithConfig[C] ?=> Setting, settings: WithConfig[C] ?=> Setting*): Command[C] = 
    given WithConfig[C] = new WithConfig[C]{}
     
    val resolvedSetting= setting
    val resolvedSettings = settings.map(param => param)

    val jointSettings = resolvedSetting +: resolvedSettings.view

    val description = jointSettings
      .filter(_.isInstanceOf[DescriptionSetting])
      .map(_.asInstanceOf[DescriptionSetting])
      .map(_.description)
      .lastOption
      .getOrElse("")

    val paramLists = jointSettings
      .filter(_.isInstanceOf[ParameterListSetting[?]])
      .map(_.asInstanceOf[ParameterListSetting[C]])
      .map(_.paramList).toSeq

    val action = jointSettings
      .filter(_.isInstanceOf[ActionSetting[?]])
      .map(_.asInstanceOf[ActionSetting[C]])
      .map(_.action)
      .lastOption
      .getOrElse(_ => 1)

    new Command(config=config,
      parameterLists=paramLists,
      action = action,
      description=description)

case class Command[C](config: C, parameterLists: Seq[ParameterList[C]], action: C => Int, description: String):
  import Command.*

  def execute(args: String*): Int = 
    val _config = collectParams(args)
    action(_config)

  private def collectParams(args: Seq[String]): C = 
    val (config, pos@Position(argIndex, flagIndex)) = parameterLists.foldLeft((this.config, Position(0, 0))){case ((config, pos), paramList) =>
      paramList.collect(config, pos, args)}

    if argIndex < args.size || flagIndex >0 then
      throw UnknownParameterException(pos)
    
    config

    


