package urwerk.cli

object Parameter:
  def apply[A]: Parameter = ???

  def apply[A](name: String, aliases: String*): Parameter = ???

  trait Capture:
    def apply(args: Seq[String]): (Option[(Parameter, Seq[String])])

trait Parameter: 
  def xx(): Unit
  def usage(text: String): Parameter = ???
  
trait Command:
  def apply(parameters: Parameter*): Command = ???

  def resolve(args: Seq[String]): String = ""

  def usage(text: String): Command = ???


object Command:

  def apply(parameters: Parameter*): Command = ???

  val run = Command(
      Parameter[String]("help", "h")
        .usage(""),
      Parameter[String]("version", "v")
    )
    (
      Parameter["run"])(
    )
    .usage("the run command").resolve(Seq("arg1", "arg2"))
