package urwerk.cli

import scala.annotation.tailrec

import Parameter.*
import ParameterList.{Setting, Label}


trait ParameterListFactory: 
  def :=[C](using ev: WithConfig[C])(param: Seq[Setting]): Command.ParameterListSetting[C]

case class Position(val argIndex: Int, val flagIndex: Int)

object ParameterList:
  sealed trait Setting

  transparent trait ParameterSetting[V, C] extends Setting

  case class Label(label: String) extends Setting

  def :=[C](using ev: WithConfig[C])(settings: Seq[Setting]): Command.ParameterListSetting[C] = 
    Command.ParameterListSetting(
      from(settings))

  def / (label: String): ParameterListFactory = 
    new ParameterListFactory:
      def :=[C](using ev: WithConfig[C])(settings: Seq[Setting]): Command.ParameterListSetting[C] = 
        Command.ParameterListSetting(from(settings :+ Label(label)))

  def from[C](settings: Seq[Setting]): ParameterList[C] = 
    new ParameterList[C](settings)
  
  def apply[C](setting: WithConfig[C] ?=> Setting, settings: WithConfig[C] ?=> Setting*): ParameterList[C] =
    given WithConfig[C] = new WithConfig[C]{}
     
    val resolvedSetting= setting
    val resolvedSettings = settings.map(param => param)
    val jointSettings = resolvedSetting +: resolvedSettings

    from(jointSettings)

  extension [C](paramList: ParameterList[C])
    def collect(config: C, args: Seq[String]): (C, Position) = 
      collect(config, Position(0, 0), args)

    def collect(config: C, pos: Position, args: Seq[String]): (C, Position) = 
      collectParams(config, paramList, pos, args)

  extension [V, C](param: Parameter[V, C])
    def applyTypeDefaultValue(config: C, pos: Position): C = 
      param.valueSpec.defaultValue match
        case Some(value) =>
          param.applyOp(config, value)
        case None =>
          throw ValueNotFoundException(pos)

    def apply(config: C, arg: String, pos: Position): C = 
      if !accept(arg) then
        throw ParameterValueRejected(pos)

      val value = param.valueSpec.convert(arg)
      param.applyOp(config, value)

    def accept(arg: String): Boolean = 
      param.acceptOp(arg)

  private def collectParams[C](config: C, 
      paramList: ParameterList[C], 
      pos: Position, 
      args: Seq[String]): (C, Position) =
    
    val collector = Collector(
      namedParams = namedParamsMap(paramList), 
      positionalParams = paramList.positionalParams, 
      trailingArgs = paramList.trailingArgs,
      pos = pos, 
      config = config, 
      args = args)

    val completed = LazyList.unfold(collector){collector =>
      if collector.completed then None
      else
        val next = collector.collect
        Some((next, next))
    }.last
    (completed.config, completed.pos)
    
  private case class Collector[C](
      namedParams: Map[String, Parameter[?, C]], 
      positionalParams: Seq[Parameter[?, C]], 
      trailingArgs: Option[TrailingArgs[C]],
      config: C,
      args: Seq[String], 
      pos: Position, 
      positionalIndex: Int = 0,
      previousName: String = "", 
      appliedParamKeys: Set[Int|String] = Set(),
      completed: Boolean = false): 
    def applyDefaultValueToPreviousName(previousName: String): Collector[C] = 
      if previousName.nonEmpty then
        namedParams.get(previousName) match
          case Some(param) =>
            copy(
              config = param.applyTypeDefaultValue(config, pos),
              appliedParamKeys = appliedParamKeys + previousName)

          case None =>
            throw IllegalStateException("this position may never be reached")
      else this

    def collect: Collector[C] = 
      val next = _collect
      if next.completed then 
        verify()
      next  

    private def _collect: Collector[C] =
      val Position(argIndex, flagIndex) = pos
      if argIndex >= args.size then
        val updated = applyDefaultValueToPreviousName(previousName)
        updated.completeWithTrailingArgs()
      
      else if isFlags(args(argIndex)) && flagIndex >= args(argIndex).size -1 then 
        copy(pos=Position(argIndex+1, 0), previousName=previousName)

      else
        val arg = args(argIndex)
        if isName(arg) then
          val updated = applyDefaultValueToPreviousName(previousName)

          val name = toName(arg)
          val paramOpt = namedParams.get(name) 
        
          if paramOpt.isDefined then   
            updated.copy(pos=Position(argIndex+1, 0), previousName=name)
        
          else
            updated.completeWithTrailingArgs()

        else if isFlags(arg) then 
          val updated = applyDefaultValueToPreviousName(previousName)

          val flags = arg.stripPrefix("-")
          val name = flags(flagIndex).toString
          val paramOpt = namedParams.get(name) 

          if paramOpt.isDefined then
            updated.copy(pos=Position(argIndex, flagIndex+1), previousName=name)
          else
            updated.completeWithTrailingArgs()
        
        else if isSeparator(arg) then
          val updated = applyDefaultValueToPreviousName(previousName)
          updated.copy(pos=Position(argIndex+1, 0), previousName = "")

        else 
          val value = stripQuotes(arg)
          if previousName.nonEmpty then
            namedParams.get(previousName) match
              case Some(param) =>
                try
                  copy(
                    config = param.apply(config, value, pos),
                    pos = Position(argIndex+1, 0),
                    previousName="", 
                    appliedParamKeys = appliedParamKeys+previousName)
                catch
                  case _: IllegalArgumentException =>
                    copy(
                      config = param.applyTypeDefaultValue(config, pos),
                      pos = Position(argIndex, 0),
                      previousName="",
                      appliedParamKeys = appliedParamKeys+previousName)
                      
                  case e: Throwable => throw e

              case None =>
                throw IllegalStateException("this position may never be reached")
          else
            if positionalIndex >= positionalParams.size then
              completeWithTrailingArgs()
            else
              val param = positionalParams(positionalIndex)
              copy(
                config= param.apply(config, value, pos), 
                pos=Position(argIndex + 1, 0), 
                positionalIndex + 1,
                previousName="",
                appliedParamKeys = appliedParamKeys + positionalIndex)
    
    private def verify(): Unit = 
      namedParams.foreach{(name, param)=>
        if param.isRequired && !appliedParamKeys.contains(name) then
          throw ParameterNotFoundException(pos, param)
      }

      positionalParams.view.zipWithIndex.foreach{(param, index)=>
        if param.isRequired && !appliedParamKeys.contains(index) then
          throw ParameterNotFoundException(pos, param)
      }

    private def completeWithTrailingArgs(): Collector[C] = 
      trailingArgs match
        case Some(trailingArgs) => 
          val Position(argIndex, flagIndex) = pos

          val tail = args.drop(argIndex)
          val nextConfig = tail.zipWithIndex.foldLeft(config){case (config, (arg, tailIndex))=>
            val nextArgIndex = argIndex+tailIndex
            val nextConfig = trailingArgs.apply(config, arg, Position(nextArgIndex, 0))
            nextConfig
          }
          copy(config = nextConfig, completed = true, pos = Position(argIndex + tail.size, 0))

        case None => 
          copy(completed=true)

  private def namedParamsMap[C](paramList: ParameterList[C]): Map[String, Parameter[?, C]] =
    namedParameters(paramList.namedParams, Map())

  @tailrec
  private def namedParameters[C](params: Seq[Parameter[?, C]], paramsMap: Map[String, Parameter[?, C]]): Map[String, Parameter[?, C]] =
    if params.isEmpty then
      paramsMap
    else
      val param = params.head
      val names = param.names
      val map = names.foldLeft(paramsMap){(map, name) =>
        map.updatedWith(name){
          case Some(_) =>
            throw IllegalArgumentException()
          case None =>
            Some(param)
        }
      }
      namedParameters(params.tail, map)

class ParameterList[C](settings: Seq[Setting]):

  lazy val namedParams: Seq[NamedParameter[?, C]] = settings
    .filter(_.isInstanceOf[NamedParameter[?, ?]])
    .map(_.asInstanceOf[NamedParameter[?, C]])

  lazy val positionalParams: Seq[PositionalParameter[?, C]] = settings
    .filter(_.isInstanceOf[PositionalParameter[?, ?]])
    .map(_.asInstanceOf[PositionalParameter[?, C]])


  lazy val trailingArgs: Option[TrailingArgs[C]] = settings
    .filter(param => param.isInstanceOf[TrailingArgs[?]])
    .map(_.asInstanceOf[TrailingArgs[C]])
    .lastOption

  lazy val label = settings
    .filter(_.isInstanceOf[Label])
    .map(_.asInstanceOf[Label].label)
    .lastOption.getOrElse("")

  def settings(settings: Seq[Setting]): ParameterList[C] =  
    ParameterList.from[C](this.settings ++ settings)

  def set(setting: WithConfig[C] ?=> Setting, settings: WithConfig[C] ?=> Setting*): ParameterList[C] =
    given WithConfig[C] = new WithConfig[C]{}

    val nextSettings = this.settings
      .appended(setting)
      .appendedAll(settings.map(setting => setting))

    ParameterList.from[C](nextSettings)
