package urwerk.command

import urwerk.test.TestBase
import Parameter.RawArg

class ParametersTest extends TestBase:
  case class Config[A](value: A)

  "int parameter" - {
    val params = Parameters(Config(0))
    import params.*

    "requires value" in {
      param[Int].converter.requireValue should be (true)
    }

    "collect value" in {
      val value = param[Int]
        .collect{case (value, config) => config.copy(value = value)}
        .doCollect("-77")

      value should be (Config(-77))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        param[Int]
          .doCollect("abc")
      }
    }
  }

  "string parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "requires value" in {
      param[String].converter.requireValue should be (true)
    }

    "collect value" in {
      val value = param[String]
        .collect{case (value, config) => config.copy(value = value)}
        .doCollect("string value")

      value should be (Config("string value"))  
    }  

    "illegal value" in {
      intercept[IllegalArgumentException]{
        param[String]
          .doCollect("-illegal arg value")
      }
    }
  }

  "unit parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "not require value" in {
      param[Unit].converter.requireValue should be (false)
    }

    "collect value" in {
      val value = param[Unit]
        .collect{case (value, config) => config.copy(value = s"Value=$value")}
        .doCollect("77")

      value should be (Config("Value=()"))  
    }
  }

  "raw arg parameter" - {
    val params = Parameters(Config(""))
    import params.*

    "not require value" in {
      param[RawArg].converter.requireValue should be (true)
    }

    "collect value" in {
      val value = param[RawArg]
        .collect{case (RawArg(value), config) => config.copy(value = value)}
        .doCollect("--param-name")

      value should be (Config("--param-name"))  
    }
  }
