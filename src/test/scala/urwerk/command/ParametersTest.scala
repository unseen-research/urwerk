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
        val p = param[Int]
        p.collectValue("")
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
        val p = param[String]
        p.collectValue("-illegal arg value")
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

    "capture name value parameters" in {
      val params = Seq(
        param[String]("name2")
          .collect((value, config) => 
            config.updated("name-2", "collected-" + value)),
        param[Int]("name1")
          .collect((value, config) => 
            config.updated("name-1", "collected-" + value))
      )

      val (config, remainingArgs) = ParameterList(Map[String, Any](),params)
        .collectParams(
          Seq("--name1", "11", "--name2", "value2", "--name3", "value3"))

      remainingArgs should be (Seq("--name3", "value3"))
      config should be (Map("name-1" -> "collected-11", "name-2" -> "collected-value2"))
    }

    "capture name parameters" in {
      val params = Seq(
        param[Unit]("name2")
          .collect((value, config) => 
            config.updated("name-2", s"$value-2")),
        param[Unit]("name1")
          .collect((value, config) => 
            config.updated("name-1", s"$value-1"))
      )

      val (config, remainingArgs) = ParameterList(Map[String, Any](),params)
        .collectParams(
          Seq("--name1", "--name2", "value2", "--name3", "value3"))

      remainingArgs should be (Seq("value2", "--name3", "value3"))
      config should be (Map("name-1" -> "()-1", "name-2" -> "()-2"))
    }

    "capture value parameters" in {
      val params = Seq(
        param[Int]
          .collect((value, config) => 
            config.updated("value-1", value)),
        param[String]
          .collect((value, config) => 
            config.updated("value-2", value))
      )

      val (config, remainingArgs) = ParameterList(Map[String, Any](),params)
        .collectParams(
          Seq("11", "value2", "value3"))

      remainingArgs should be (Seq("value3"))
      config should be (Map("value-1" -> 11, "value-2" -> "value2"))
    }

  }
