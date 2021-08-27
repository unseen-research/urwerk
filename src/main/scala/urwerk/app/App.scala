package urwerk.app

import scala.concurrent.Future

trait App(op: Seq[String] => Future[?])