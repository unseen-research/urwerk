package urwerk.app

import java.util.concurrent.Callable

import picocli.CommandLine.Command
import picocli.CommandLine.Option
import picocli.CommandLine.ScopeType

@Command(name = "Urwerk", mixinStandardHelpOptions = true, version = Array("1.0.x"))
given Callable[Int] with Main.Application with
  @Option(names = Array("--global"), scope = ScopeType.INHERIT)
  var global: Int = 0

  override def toString = "not test"

  def call(): Int = 7

given Seq[Main.Command] = Seq()

