package urwerk.cli

import urwerk.test.TestBase
import Cmd.*

class CmdTest extends TestBase:
  
  "value setting" in {
    case class Config()

    val cmd = Cmd[Config](Value / "any-name" := "any-value", 
      Value / "other-name" := "other-value")

    cmd.settings should be(Seq(ValueBinding("any-name", "any-value"), ValueBinding("other-name", "other-value")))
  }

  "action setting" in {
    case class Config(value: Int)

    val cmd = Cmd[Config](Action := {config => config.value + 1})

    cmd.action(Config(77)) should be (78)
  }

  "execute with values" in {
    import scala.deriving.Mirror
    import scala.compiletime.summonAll


    case class Config(a: String, b: Int, c: Boolean)

    val mirror = summon[Mirror.Of[Config]]    
    type ValueOfs = Tuple.Map[mirror.MirroredElemLabels, ValueOf]

    val valueOfs = summonAll[ValueOfs]

    println(s"TTTTTTTTJ $valueOfs")
  

    val cmd = Cmd[Config](Action := {config => config})

//    cmd.execute() should be (Config("any-value", 7, true))
  }
