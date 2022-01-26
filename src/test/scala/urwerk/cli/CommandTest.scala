package urwerk.cli

import urwerk.cli.Parameter.param
import urwerk.test.TestBase

case class Config(value: String = "")

class CommandTest extends TestBase:
  
  "execute with apply returning int" in {
    val cmd = Command(Config("7")).apply{config => 
      config.value.toInt
    }

    cmd.execute() should be (7)
  }

  "execute with apply returning unit" in {
    val cmd = Command(Config("7")).apply{config => 
      "something"
    }

    cmd.execute() should be (0)
  }

  "execute with parameter" in {
    val cmd = Command(Config("7")).parameterList(
        param[String]("param", "p")
          .onApply((value, config) => Config(value)))
      .apply(config => config.value.toInt)
    
    cmd.execute("--param", "8") should be (8)
  }
