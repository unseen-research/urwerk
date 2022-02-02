package urwerk.cli

import urwerk.cli.ParameterList.Label
import urwerk.cli.Parameter.param
import urwerk.test.TestBase

import ParameterList.Position


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
    val params = ParameterList[Seq[Int]](
      param[Int]
        .apply{case (value, config) => config :+ value}
    )

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
    val params = ParameterList[Set[String]]{
        param[Boolean]("param", "alias")
          .apply{case (value, config) => config + s"param-$value"}} 

    "with explicit true value" in {
      params.collect(Set(), Seq("--param", "true")) should be ((Set("param-true"), Position(2, 0)))
    }

    "with explicit false value" in {
      params.collect(Set(), Seq("--param", "false")) should be ((Set("param-false"), Position(2, 0)))
    }

    "without value defaults to implicit true value" in {
      params.collect(Set(), Seq("--param")) should be ((Set("param-true"), Position(1, 0)))
    }

    "followed by name" in {
      params.collect(Set(), Seq("--param", "--any-value")) should be ((Set("param-true"), Position(1, 0)))
    }

    "followed by none boolean value defaults to implicit true value" in {
      params.collect(Set(), Seq("--param", "any")) should be ((Set("param-true"), Position(1, 0)))
    }

    "followed by separator followed by boolean defaults to implicit true value" in {
      val params2 = params.add(
        param[Boolean]
          .apply{case (value, config) => config + s"positional-$value"})
      
      params2.collect(Set(), Seq("--param", "--", "false")) should be ((Set("param-true", "positional-false"), Position(3, 0)))
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

  "positional values" in {
    val params = ParameterList[Seq[Int]](
        param[Int]
          .apply{case (value, config) => config :+ value},
        param[Int]
          .apply{case (value, config) => config :+ value})
      .add(
        param[Int]
          .apply{case (value, config) => config :+ value},
        param[Int]
          .apply{case (value, config) => config :+ value})

    params.collect(Seq(), Seq("1", "2", "3", "4", "5")) should be (Seq(1, 2, 3, 4), Position(4, 0))
  }

  "required named param" in {
    val params = ParameterList[Seq[Int]](
        param[Int]("number")
          .required
          .apply{case (value, config) => config :+ value})

    val paramException = intercept[ParameterException]{
      params.collect(Seq(), Seq())
    }

    paramException.getCause() shouldBe a[ParameterNotFoundException]
  }

  "multiple params" in {
    val params = ParameterList[Set[String]](
      param[Boolean]("param1", "a")
        .apply{case (value, config) => config + s"param1-$value"},
      param[Int]("param2", "b")
        .apply{case (value, config) => config + s"param2-$value"},
      param[Int]
        .apply{case (value, config) => config + s"pos-int-$value"},
      param[String]("param3", "c")
        .apply{case (value, config) => config + s"param3-$value"},
      param[String]
        .apply{case (value, config) => config + s"pos-string-$value"}, 
      param[Boolean]
        .apply{case (value, config) => config + s"pos-boolean-$value"})

    val (config, pos) = params.collect(Set(), Seq("--param1", "true", "77", "--param1", "false", "positional-value", "true", "--param3", "value3", "--param2", "42", "tail1", "tail2"))
    
    config should be(Set("pos-boolean-true", "param1-false", "pos-int-77", "param3-value3", "param1-true", "pos-string-positional-value", "param2-42"))
    pos should be (Position(11, 0))
  }

  "add label" in {
    val params = ParameterList[Set[String]](
      Label("PARAMS1"), Label("PARAMS2"))

    params.label should be("PARAMS2")
  }

  "with config" in {
    given WithConfig[String] = config
    
    val params = Seq(
      param[Boolean]("param1", "a")
        .apply{case (value, config) => config + s"param1-$value"},
      param[Int]("param2", "b")
        .apply{case (value, config) => config + s"param2-$value"})

    val paramList = ParameterList.from(params)
    paramList.params should be(params)
  }

  "create parameter list command setting" - {
    given WithConfig[String] = config

    val params = Seq(
      param[Boolean]("param1", "a")
        .apply{case (value, config) => config + s"param1-$value"},
      param[Int]("param2", "b")
        .apply{case (value, config) => config + s"param2-$value"})

      "without label" in {
        val setting = ParameterList := params

        setting.label should be("")
        setting.paramList.params should be (params)
      }

      "with label" in {
        val setting = ParameterList / "LABEL" := params

        setting.label should be("LABEL")
        setting.paramList.params should be (params)
      }
  }
   
