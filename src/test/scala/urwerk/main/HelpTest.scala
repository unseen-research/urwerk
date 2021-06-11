package urwerk.main

import urwerk.test.TestBase
import urwerk.source.TestOps.*

class HelpTest extends TestBase:
  "print help" in {
      val out = Help(Seq("--help"))
        .concat.map(_.getOrElse("")).mkString.block

      out should be (help)
  }

  "is not help" in {
    optionalProbe(
        Help(Seq("--other")))
      .verifyComplete()
  }