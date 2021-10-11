package urwerk.app.command

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
        .collectValue(Config(0), "-77", Position(4, 5))

      value should be (Config(-77))
    }

    "illegal value" in {
      val ex = intercept[IllegalValueException]{
        val p = param[Int]
        p.collectValue(Config(0), "", Position(4, 5))
      }
      ex.position should be(Position(4, 5))
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
        .collectValue(Config(""), "string value", Position(4, 5))

      value should be (Config("string value"))
    }

    "illegal value" in {
      intercept[IllegalArgumentException]{
        val p = param[String]
        p.collectValue(Config(""), "-illegal arg value", Position(4, 5))
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
        .collectValue(Config(""), "", Position(4, 5))

      value should be (Config("Value=()"))
    }
  }

  import Parameters.ParameterList

  "parameter list" - {
    val params = Parameters[Seq[String]]
    import params.*

    "name value parameters" in {
      val params = Seq(
        param[String]("name2")
          .collect((value, config) =>
            config :+ "name2-" + value),
        param[Int]("name1")
          .collect((value, config) =>
            config :+ "name1-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(),
          Seq("--name1", "11", "--name2", "value2", "--name3", "value3"))

      pos should be (Position(4, 0))
      config should be (Seq("name1-11", "name2-value2"))
    }

    "name parameters" in {
      val params = Seq(
        param[Unit]("name2")
          .collect((value, config) =>
            config :+ "name2-" + value),
        param[Unit]("name1")
          .collect((value, config) =>
            config :+ "name1-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(),
          Seq("--name1", "--name2", "value2", "--name3", "value3"))

      pos should be (Position(2, 0))
      config should be (Seq("name1-()", "name2-()"))
    }

    "name parameter with required but missing value" in {
      val params = Seq(
        param[Unit]("name1"),
        param[String]("name2")
      )

      val ex = intercept[MissingValueException]{
        ParameterList(params).collectParams(Seq(),
          Seq("--name1", "--name2"))}

      ex.position should be(Position(1, 0))
    }

    "value parameters" in {
      val params = Seq(
        param[Int]
          .collect((value, config) =>
            config :+ "pos1-" + value),
        param[String]
          .collect((value, config) =>
            config :+ "pos2-" + value),
        param[String]
          .collect((value, config) =>
            config :+ "pos3-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq("11", "value2", "--name", "value3"))

      pos should be (Position(2, 0))
      config should be (Seq("pos1-11", "pos2-value2"))
    }

    "raw value parameters" in {
      val params = Seq(
        param[String]
          .accept(_ => true)
          .arity(0, Int.MaxValue)
          .collect((value, config) =>
            config :+ value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq("11", "value2", "--name", "value3"))

      pos should be (Position(4, 0))
      config should be (Seq("11", "value2", "--name", "value3"))
    }

    "positional max arity" in {
      val params = Seq(
        param[String]
          .arity(0, 2)
          .collect((value, config) =>
            config :+ "a-" + value),
        param[String]
          .arity(0, 1)
          .collect((value, config) =>
            config :+ "b-" + value),
        param[String]
          .arity(0, 2)
          .collect((value, config) =>
            config :+ "c-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq("value1", "value2", "value3", "value4", "value5", "value6"))

      pos should be (Position(5, 0))
      config should be (Seq("a-value1", "a-value2", "b-value3", "c-value4", "c-value5"))
    }

    "positional min arity missed" in {
      val params = Seq(
        param[String]("name1")
          .collect((value, config) => config),
        param[String]
          .arity(3, 4)
          .label("STRING_PARAM")
          .collect((value, config) => config :+ "b-" + value)
      )

      val ex = intercept[MissingParameterException] {
        ParameterList(params)
          .collectParams(Seq(), Seq("--name1", "value1", "value2", "value3"))
      }
      ex.labelOrName should be ("STRING_PARAM")
      ex.requiredArity should be (3)
      ex.repetition should be (2)
      ex.position should be (Position(4, 0))
    }

    "named max arity" in {
      val params = Seq(
        param[String]("name-a")
          .arity(0, 2)
          .collect((value, config) =>
            config :+ "a-" + value),
        param[String]("name-b")
          .arity(0, 1)
          .collect((value, config) =>
            config :+ "b-" + value),
        param[String]("name-c")
          .arity(0, 2)
          .collect((value, config) =>
            config :+ "c-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq("--name-c", "value1", "--name-a", "value2", "--name-b", "value3", "--name-a", "value4", "value5"))

      pos should be (Position(8, 0))
      config should be (Seq("c-value1", "a-value2", "b-value3", "a-value4"))
    }

    "named min arity missed" in {
      val params = Seq(
        param[String]("name1")
          .collect((value, config) => config)
          .arity(3, 4),
      )

      val ex = intercept[MissingParameterException] {
        ParameterList(params)
          .collectParams(Seq(), Seq("--name1", "value1", "--name1", "value2", "value3"))
      }
      ex.labelOrName should be ("name1")
      ex.requiredArity should be (3)
      ex.repetition should be (2)
    }

    "named max arity exceeded" in {
      val params = Seq(
        param[String]("any-name")
          .collect((value, config) => config)
          .arity(0, 2),
      )

      val ex = intercept[ArityExceededException] {
        ParameterList(params)
          .collectParams(Seq(), Seq("--any-name", "value1", "--any-name", "value2", "--any-name", "value3"))
      }
      ex.name should be ("any-name")
      ex.maxArity should be (2)
    }

    "flags without value" in {
      val params = Seq(
        param[Unit]("a", "A")((value, config) =>
            config :+ "a-" + value)
          .arity(0, 77),
        param[Unit]("b", "B")((value, config) =>
            config :+ "b-" + value)
          .arity(0, 77),
        param[String]("c", "C")((value, config) =>
            config :+ "c-" + value)
          .arity(0, 77)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq("-aAbBc", "valueC", "tail"))

      pos should be (Position(2, 0))
      config should be (Seq("a-()", "a-()", "b-()", "b-()", "c-valueC"))
    }

    "flags with required but missing value" in {
      val params = Seq(
        param[Unit]("a")((value, config) =>
            config :+ "a-" + value),
        param[Unit]("b", "B")((value, config) =>
            config :+ "b-" + value),
        param[String]("c", "C")((value, config) =>
            config :+ "c-" + value)
      )

      val ex = intercept[MissingValueException]{
        ParameterList(params)
          .collectParams(Seq(), Seq("-abc"))
      }
      ex.position should be(Position(0, 2))
    }

    "default values" in {
      val params = Seq(
        param[Unit]("name-a", "a", "A")
          .default(())
          .collect((value, config) =>
            config :+ "a-" + value),
        param[Int]
          .default(42)
          .collect((value, config) =>
            config :+ "b-" + value),
        param[String]("name-c", "c", "C")
          .default("default-value")
          .collect((value, config) =>
            config :+ "c-" + value)
      )

      val (config, pos) = ParameterList(params)
        .collectParams(Seq(), Seq())

      pos should be (Position(0, 0))
      config.toSet should be (Set("a-()", "b-42", "c-default-value"))
    }

    "fail when exception is thrown while collect value" in {
      val params = Seq(
        param[String]("name1")((value, config) => throw IllegalStateException("test message"))
      )

      val ex = intercept[CollectValueException] {
        ParameterList(params)
          .collectParams(Seq(), Seq("--name1", "value1"))
      }

      val cause = ex.getCause()
      cause.getMessage should be ("test message")
      cause shouldBe a[IllegalStateException]
    }
  }

end ParametersTest