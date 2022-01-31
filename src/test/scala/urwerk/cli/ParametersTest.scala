package urwerk.cli

import urwerk.cli.Parameter.param
import urwerk.test.TestBase

class ParametersTest extends TestBase:
  
  Seq(("-", true), ("--", true), ("---", true), ("-n", false), ("--name", false), ("value", false), ("'--'", false), ("\"--\"", false)).foreach{(givenArg, result)=>
    import ParameterList.isSeparator

    s"given arg $givenArg is separator $result" in {
      isSeparator(givenArg) should be (result)
    }}

  Seq(("-n", true), ("--name", true), ("---name", false), ("-name", false), ("-", false), ("--", false)).foreach{(givenArg, result)=>
    import ParameterList.isName

    s"given arg $givenArg is name $result" in {
      isName(givenArg) should be (result)
    }}

  Seq(("-flags", true), ("-f", true), ("-123", false), ("--name", false), ("-", false), ("--", false)).foreach{(givenArg, result)=>
    import ParameterList.isFlags

    s"given arg $givenArg is flags $result" in {
      isFlags(givenArg) should be (result)
    }}


  Seq(("-n", "n"), ("--n", "n"), ("--name", "name")).foreach{(givenArg, result)=>
    import ParameterList.toName

    s"given arg $givenArg  name $result" in {
      toName(givenArg) should be (result)
    }}

  Seq(("value", "value"), ("\"\"", ""), ("''", ""), ("\"--\"", "--"), ("'--'", "--"), ("\"--name\"", "--name"), ("'--name'", "--name"), ("\"value", "\"value"), ("'value", "'value"))
    .foreach{(givenArg, result)=>
    import ParameterList.stripQuotes

    s"given arg $givenArg unquoted $result" in {
      stripQuotes(givenArg) should be (result)
    }}

  "collect positional value arg" - {
    val params = ParameterList[Seq[Int]]{
      param[Int]
        .apply{case (value, config) => config :+ value}
    }

    "followed by nothing" in {
      params.collect(Seq(), Seq("77")) should be ((Seq(77), Position(1, 0)))
    }

    "negative number value starting with minus which is flags prefix" in {
      params.collect(Seq(), Seq("-77")) should be ((Seq(-77), Position(1, 0)))
    }

    "followed by value" in {
      params.collect(Seq(), Seq("77", "88")) should be ((Seq(77), Position(1, 0)))
    }

    "followed by name" in {
      params.collect(Seq(), Seq("77", "88")) should be ((Seq(77), Position(1, 0)))
    }

    "followed by flags" in {
      params.collect(Seq(), Seq("77", "-flags")) should be ((Seq(77), Position(1, 0)))
    }
  }

  "collect named param" - {
    val params = ParameterList[Seq[String]]{
        param[String]("param", "alias", "p")
          .apply{case (value, config) => config :+ value}} 

    "with primary name followed by nothing" in {
      params.collect(Seq(), Seq("--param", "any-value")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with primary name followed by value" in {
      params.collect(Seq(), Seq("--param", "any-value", "other")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with primary name followed by name" in {
      params.collect(Seq(), Seq("--param", "any-value", "--other")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with primary name followed by flags" in {
      params.collect(Seq(), Seq("--param", "any-value", "-other")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with alias name" in {
      params.collect(Seq(), Seq("--alias", "any-value", "--other")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with short name" in {
      params.collect(Seq(), Seq("-p", "any-value", "--other")) should be ((Seq("any-value"), Position(2, 0)))
    }

    "with single quoted value" in {
      params.collect(Seq(), Seq("--alias", "'--any-value--'")) should be ((Seq("--any-value--"), Position(2, 0)))
    }

    "with double quoted value" in {
      params.collect(Seq(), Seq("--alias", "\"--any-value--\"")) should be ((Seq("--any-value--"), Position(2, 0)))
    }

  }

  "collect named boolean param" - {
    val params = ParameterList[Seq[Boolean]]{
        param[Boolean]("param", "alias")
          .apply{case (value, config) => config :+ value}} 

    "with primary name with true" in {
      params.collect(Seq(), Seq("--param", "true")) should be ((Seq(true), Position(2, 0)))
    }

    "with primary name with false" in {
      params.collect(Seq(), Seq("--param", "false")) should be ((Seq(false), Position(2, 0)))
    }

    "with primary name without value" in {
      params.collect(Seq(), Seq("--param")) should be ((Seq(true), Position(1, 0)))
    }

    "with primary name followed by name" in {
      params.collect(Seq(), Seq("--param", "--any-value")) should be ((Seq(true), Position(1, 0)))
    }

    "with primary name followed by none boolean value" in {
      params.collect(Seq(), Seq("--param", "any")) should be ((Seq(true), Position(1, 0)))
    }
  }

  "joined flags" - {
    val params = ParameterList[Set[String]](
      param[Boolean]("a-param", "a")
        .apply{case (value, config) => config + s"a-$value"},
      param[Boolean]("b-param", "b")
        .apply{case (value, config) => config + s"b-$value"},
      param[Boolean]("c-param", "c")
        .apply{case (value, config) => config + s"c-$value"},
      param[String]("d-param", "d")
        .apply{case (value, config) => config + s"d-$value"})

    "without value" in {
      params.collect(Set(), Seq("-abc")) should be ((Set("a-true", "b-true", "c-true"), Position(1, 0)))
    }

    "with value for last flag" in {
      params.collect(Set(), Seq("-abcd", "any-value")) should be ((Set("a-true", "b-true", "c-true", "d-any-value"), Position(2, 0)))
    }
  }
