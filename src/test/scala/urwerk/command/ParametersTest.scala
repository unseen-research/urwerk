package urwerk.command

import urwerk.test.TestBase
import Parameter.RawArg
import Parameters.*

class ParametersTest extends TestBase:
  case class Config[A](value: A)

  "int parameter" - {
    val params = Parameters(Config(0))
    import params.*

    "requires value" in {
      param[Int].valueTypeOps.requireValue should be (true)
    }

    "collect value" in {
      val value = param[Int]
        .collect{case (value, config) => config.copy(value = value)}
        .collectValue("-77")

      value should be (Config(-77))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        param[Int]
          .collectValue("abc")
      }
    }
  }

  "string parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "requires value" in {
      param[String].valueTypeOps.requireValue should be (true)
    }

    "collect value" in {
      val value = param[String]
        .collect{case (value, config) => config.copy(value = value)}
        .collectValue("string value")

      value should be (Config("string value"))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        param[String]
          .collectValue("-illegal arg value")
      }
    }
  }

  "unit parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "not require value" in {
      param[Unit].valueTypeOps.requireValue should be (false)
    }

    "collect value" in {
      val value = param[Unit]
        .collect{case (value, config) => config.copy(value = s"Value=$value")}
        .collectValue("77")

      value should be (Config("Value=()"))  
    }
  }

  "raw arg parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "not require value" in {
      param[RawArg].valueTypeOps.requireValue should be (true)
    }

    "collect value" in {
      val value = param[RawArg]
        .collect{case (RawArg(value), config) => config.copy(value = value)}
        .collectValue("--param-name")

      value should be (Config("--param-name"))  
    }
  }

  import Parameters.ParameterList

  "parameter list" - {

    val params = Parameters(Map[String, Any]())
    import params.*

    "capture named parameters" in {
      val args = Seq("--name1", "value1", "--name2", "value2", "--name3", "value3")

      val config = ParameterList(Seq(param[String], param[String]))
        .collectParams(args)

        
    }
  }
