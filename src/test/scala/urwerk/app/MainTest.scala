package urwerk.app

import urwerk.test.TestBase
import urwerk.source.Source
import urwerk.io.Path
import urwerk.source.Optional

class TestMain extends urwerk.app.Main(Source.empty)

object Exec:
  def apply(path: Path, args: Seq[String]): Exec = Exec(path, args, None, Map())

case class Exec(path: Path, args: Seq[String], cwd: Option[Path], env: Map[String, String])

extension (exec: Exec)
  def toSource: Optional[(Source[String], Source[String])] =
    val path = exec.path.toString

    val processBuilder = new ProcessBuilder(path, "myArg1", "myArg2")
      

    Optional.empty

class MainTest extends TestBase:
  "run main" in {


  }