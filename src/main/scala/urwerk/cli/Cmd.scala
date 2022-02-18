package urwerk.cli


object Cmd:
  sealed trait Setting[C]

  case class ValueBinding[C, V](name: String, value: V) extends Setting[C]

  case class ValueName[C](name: String):
    def :=[V] (value: V): ValueBinding[C, V] = ValueBinding(name, value)
   
  object Value:
    def / [C] (name: String): ValueName[C] = ValueName(name)

  
  case class Action[C](action: C => Any) extends Setting[C]:
    def apply(config: C): Any = action(config)
  
  object Action:

    def :=[C] (action: C => Any): Action[C] = Action(action)

  def apply[C <: Product](setting: Setting[C], settings: Setting[C]*): Cmd[C] = 
    ///val valueOfs = summonAll[ValueOfs]
    

    new Cmd(setting +: settings)

  extension [C <: Product](cmd: Cmd[C])
    def execute(args: String*): Any = 
      val action = cmd.action
      //val mirror = cmd.mirror

    //val types = summon[mirror.MirroredElemTypes =:= (String, Int, Boolean)]
    
   // val valueOfs = summonAll[ValueOfs]
      ???

end Cmd

class Cmd[C <: Product](val settings: Seq[Cmd.Setting[C]]):
  import Cmd.*

  //val mirror = summon[Mirror.Of[C]]

  lazy val action: C => Any = 
    settings.collect{case Action(action) => action}.last
