package urwerk.cli

import urwerk.test.TestBase
import OptionSpec.OverflowStrategy.*

class CliTest extends TestBase:

  "zero arity option without tail" in {
    val (opts, tail) = Seq("--help").extractOptions(option[Unit]("help", "h"))
    tail shouldBe empty
    opts should be (Map("help" -> ()))
  }

  "take zero arity with trailing value" in {
    val (opts, tail) = Seq("--help", "value").extractOptions(option[Unit]("help", "h"))
    tail should be(Seq("value"))
    opts should be (Map("help" -> ()))
  }

  "zero arity option with shortcut without tail" in {
    val (opts, tail) = Seq("-h").extractOptions(option[Unit]("help", "h"))
    tail shouldBe empty
    opts should be (Map("help" -> ()))
  }

  "zero arity option with flag with trailing flags" in {
    val (opts, tail) = Seq("-habc").extractOptions(option[Unit]("help", "x", "xyz", "h"))
    tail should be (Seq("-abc"))
    opts should be (Map("help" -> ()))
  }

  "zero arity option provides value" in {
    val spec = option[Boolean]("help", "h")
      .arity(0, 0)
      .map(_ => true)
    val (opts, tail) = Seq("-h").extractOptions(spec)

    tail shouldBe empty
    opts should be (Map("help" -> true))
  }

  "zero arity with trailing value provides value" in {
    val spec = option[Boolean]("help", "h")
      .arity(0, 0)
      .map(_ => false)

    val (opts, tail) = Seq("--help", "value").extractOptions(spec)
    tail should be(Seq("value"))
    opts should be (Map("help" -> false))
  }

  "single value option" in {
    val opt = option[String]("option", "o").map(_.toUpperCase)
    val (opts, tail) = Seq("--option", "value").extractOptions(opt)
    tail shouldBe empty
    opts should be (Map("option" -> "VALUE"))
  }

  "single value overflow strategy drop first" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .overflowStrategy(DropFirst)

    val (opts, tail) = Seq("--option", "value1", "--option", "value2", "other-value").extractOptions(opt)
    tail should be(Seq("other-value"))
    opts should be (Map("option" -> "VALUE2"))
  }

  "single value overflow strategy drop last" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .overflowStrategy(DropLast)

    val (opts, tail) = Seq("--option", "value1", "--option", "value2", "other-value").extractOptions(opt)
    tail should be(Seq("other-value"))
    opts should be (Map("option" -> "VALUE1"))
  }

  "single value overflow strategy reject" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .overflowStrategy(Reject)

    val (opts, tail) = Seq("--option", "value1", "--option", "value2", "other-value").extractOptions(opt)
    tail should be(Seq("--option", "value2", "other-value"))
    opts should be (Map("option" -> "VALUE1"))
  }

  "single value overflow strategy fail" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .overflowStrategy(Fail)

    intercept[ArityExceededException]{
      Seq("--option", "value1", "--option", "value2", "other-value").extractOptions(opt)}
  }

  "single value option with trailing value" in {
    val opt = option[String]("option", "o").map(_.toUpperCase)
    val (opts, tail) = Seq("--option", "value", "other-value").extractOptions(opt)
    tail should be(Seq("other-value"))
    opts should be (Map("option" -> "VALUE"))
  }

  "multi value option" in {
    val opt = option[String]("option", "o").map(_.toUpperCase).arity(0, 3)
    val (opts, tail) = Seq("--option", "value1", "-o", "value2").extractOptions(opt)
    tail shouldBe empty
    opts should be (Map("option" -> Seq("VALUE1", "VALUE2")))
  }

  "multi value option with overflow strategy drop first" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .arity(0, 2)
      .overflowStrategy(DropFirst)
    val (opts, tail) = Seq("--option", "value1", "-o", "value2", "--option", "value3").extractOptions(opt)
    tail shouldBe empty
    opts should be (Map("option" -> Seq("VALUE2", "VALUE3")))
  }

  "multi value option with overflow strategy drop last" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .arity(0, 2)
      .overflowStrategy(DropLast)
    val (opts, tail) = Seq("--option", "value1", "-o", "value2", "--option", "value3").extractOptions(opt)
    tail shouldBe empty
    opts should be (Map("option" -> Seq("VALUE1", "VALUE2")))
  }

  "multi value option with overflow strategy reject" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .arity(0, 2)
      .overflowStrategy(Reject)
    val (opts, tail) = Seq("--option", "value1", "-o", "value2", "--option", "value3").extractOptions(opt)
    tail should be (Seq("--option", "value3"))
    opts should be (Map("option" -> Seq("VALUE1", "VALUE2")))
  }

  "multi value option with overflow strategy fail" in {
    val opt = option[String]("option", "o")
      .map(_.toUpperCase)
      .arity(0, 2)
      .overflowStrategy(Fail)
    intercept[ArityExceededException]{
      Seq("--option", "value1", "-o", "value2", "--option", "value3").extractOptions(opt)
    }
  }

  "missing single value option provides default value" in {
    val opt = option[String]("option", "o").default("default-value")
    val (opts, tail) = Seq("--other-option", "value", "other-value").extractOptions(opt)
    tail should be(Seq("--other-option", "value", "other-value"))
    opts should be (Map("option" -> "default-value"))
  }

  "missing multi value option provides default value seq" in {
    val opt = option[String]("option", "o").default("default-value").arity(0, 3)
    val (opts, tail) = Seq("--other-option", "value", "other-value").extractOptions(opt)
    tail should be(Seq("--other-option", "value", "other-value"))
    opts should be (Map("option" -> Seq("default-value")))
  }

  "missing required option without default value fails with no such option" in {
    val opt = option[String]("option", "o").arity(1, 1)

    val exception = intercept[NoSuchOptionException] {
      Seq("--other-option", "value", "other-value").extractOptions(opt)
    }
    exception.spec.name should be ("option")
  }

  "missing option value" in {
    val opt = option[String]("option", "o")

    val exception = intercept[NoSuchValueException] {
      Seq("--option").extractOptions(opt)
    }
    exception.spec.name should be ("option")
  }

  "missing option value with following other option" in {
    val opt = option[String]("option", "o")

    val exception = intercept[NoSuchValueException] {
      Seq("--option", "--other-option").extractOptions(opt)
    }
    exception.spec.name should be ("option")
  }

  "next value" in {
    val (value, tail) = nextValue(Seq("value", "v2", "v3"))
    tail should be (Seq("v2", "v3"))
    value should be ("value")
  }

  "next value on flags" in {
    val (value, tail) = nextValue(Seq("-flags", "v1", "v2"))
    tail should be (Seq("-flags", "v1", "v2"))
    value should be ("")
  }