package urwerk.command

import urwerk.test.TestBase
import Parameters.*

class ParametersTest extends TestBase:
  case class Config[A](value: A)

  "int parameter" - {
    val params = Parameters[Config[Int]]
    import params.*

    "requires value" in {
      param[Int].valueRequired should be (true)
    }

    "collect value" in {
      val value = param[Int]
        .collect{case (value, config) => config.copy(value = value)}
        .collectValue(Config(0), "-77")

      value should be (Config(-77))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        val p = param[Int]
        p.collectValue(Config(0), "")
      }
    }
  }

  "string parameter" - {
    val params = Parameters[Config[String]]()
    import params.*

    "requires value" in {
      param[String].valueRequired should be (true)
    }

    "collect value" in {
      val value = param[String]
        .collect{case (value, config) => config.copy(value = value)}
        .collectValue(Config(""), "string value")

      value should be (Config("string value"))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        val p = param[String]
        p.collectValue(Config(""), "-illegal arg value")
      }
    }
  }

  "unit parameter" - {
    val params = Parameters[Config[String]]
    import params.*

    "not require value" in {
      param[Unit].valueRequired should be (false)
    }

    "collect value" in {
      val value = param[Unit]
        .collect{case (value, config) => config.copy(value = s"Value=$value")}
        .collectValue(Config(""), "")

      value should be (Config("Value=()"))  
    }
  }

  import Parameters.ParameterList

  "parameter list" - {
    val params = Parameters[Map[String, Any]]
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

      val (config, remainingArgs) = ParameterList(params)
        .collectParams(Map(),
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

      val (config, remainingArgs) = ParameterList(params)
        .collectParams(Map(),
          Seq("--name1", "--name2", "value2", "--name3", "value3"))

      remainingArgs should be (Seq("value2", "--name3", "value3"))
      config should be (Map("name-1" -> "()-1", "name-2" -> "()-2"))
    }

    "collect value parameters" in {
      val params = Seq(
        param[Int]
          .collect((value, config) => 
            config.updated("value-1", value)),
        param[String]
          .collect((value, config) => 
            config.updated("value-2", value)),
        param[String]
          .collect((value, config) => 
            config.updated("value-3", value))
      )

      val (config, remainingArgs) = ParameterList(params)
        .collectParams(Map(), Seq("11", "value2", "--name", "value3"))

      remainingArgs should be (Seq("--name", "value3"))
      config should be (Map("value-1" -> 11, "value-2" -> "value2"))
    }

    "collect raw args" in {
      val params_ = Parameters[Seq[String]]
      import params_.*

      val params = Seq(
        param[String]
          .accept(_ => true)
          .arity(0, Int.MaxValue)
          .collect((value, config) => 
            config :+ value)
      )

      val (config, remainingArgs) = ParameterList(params)
        .collectParams(Seq(), Seq("11", "value2", "--name", "value3"))

      remainingArgs should be (Seq())     
      config should be (Seq("11", "value2", "--name", "value3")) 
    }
  }
