package urwerk.app.command

import urwerk.test.TestBase
import urwerk.app.command.Parameters.ParameterException
import urwerk.app.command.Parameters.MissingParameterException
import urwerk.app.command.Parameters.Position
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.app.command.Parameters.UnexpectedParameterException

import Command.*

class CommandTest extends TestBase:

  val params = Parameters[Seq[String]]
  import params.*

  "command with multiple param lists" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1")
          .onApply((value, config) =>
            config :+ s"name1:$value"),
        param[Int]("name2")
          .onApply((value, config) =>
            config :+ s"name2:$value"))
      .params(
        param[String]
          .onApply((value, config) =>
            config :+ s"cmd:$value")
          .accept(_ == "command"))
      .params(
        param[Int]("name3")
          .onApply((value, config) =>
            config :+ s"name3:$value"))
      .onApply(config =>
        Source.from(config).map(Right(_)))

    val result = cmd.create(Seq("--name2", "55", "--name1", "value1", "command", "--name3", "77"))
      .flatMap(src => src)
      .flatMap{
        case Right(elem) =>  Source(elem)
        case _ => ???}
      .toSeq.block

    result should be (Seq("name2:55", "name1:value1", "cmd:command", "name3:77"))
  }

  "command with multiple param lists with undefined trailing args" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1"))
      .params(
        param[Int]("name2"))
    val ex = intercept[UnexpectedParameterException]{
      cmd.create(Seq("--name1", "value1", "--name2", "77", "undefined1", "undefined2"))
        .block
    }
    ex.position should be (Position(4, 0))
  }

  "command with multiple param lists with undefined trailing flags" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1"))
      .params(
        param[Int]("name2"),
        param[Unit]("a"),
        param[Unit]("b"),
        param[Unit]("c"))
    val ex = intercept[UnexpectedParameterException]{
      cmd.create(Seq("--name1", "value1", "--name2", "77", "-abcUndefined", ""))
        .block
    }
    ex.position should be (Position(4, 3))
  }

  "command with multiple param lists and trailing args" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("name1")
          .onApply((value, config) =>
          config :+ s"name1-$value"),
        param[Int]("name2")
          .onApply((value, config) =>
            config :+ s"name2-$value"))
      .params(
        param[String]
          .onApply((value, config) =>
            config :+ s"cmd-$value")
          .accept(_ == "command"))
      .params(
        param[String]
          .onApply((value, config) =>
            config :+ s"param-$value")
          .arity(0, 77)
          .accept(_ => true))
      .onApply(config =>
        Source.from(config).map(Right(_)))

    val result = cmd.create(Seq("--name2", "55", "--name1", "value1", "command", "--name", "value"))
      .flatMap(src => src)
      .flatMap{
        case Right(elem) =>  Source(elem)
        case _ => ???}
      .toSeq.block

    result should be (Seq("name2-55", "name1-value1", "cmd-command", "param---name", "param-value"))
  }

  "command does not match" in {
    val cmd = Command("command", Seq[String]())
      .params(
        param[String]("param1")
          .onApply((value, config) =>
            config :+ s"param1-$value")
          .default("default-value")
      )
      .params(
        param[Int]
          .onApply((value, config) =>
            config :+ s"int-param-$value")
          .arity(1, 1)
      )

    val result = cmd.create(Seq("value1", "value2"))
       .toSeq.block

    result should be (Seq())
  }
