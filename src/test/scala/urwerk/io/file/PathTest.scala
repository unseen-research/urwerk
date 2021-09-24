package urwerk.io.file

import java.nio.file.Paths

import urwerk.test.TestBase

class PathTest extends TestBase:
  "absolute path from elements" in {
    val path = Path("/", "abc", "def")
    path.isAbsolute should be (true)
    path should be (Paths.get("/abc/def"))
  }

  "cwd" in {
    Cwd should be (Path(sys.props("user.dir")))
    Cwd.isAbsolute should be (true)
  }

  "operator /" in {
    Path("/abc") / "def" / "ghi" should be (Paths.get("/abc/def/ghi"))
  }

  "relative path from elements" in {
    val path = Path("abc", "def")
    path.isAbsolute should be (false)
    path should be (Paths.get("abc/def"))
  }

  "to seq with absolute path" in {
    Path("/abc/def").toSeq should be(Seq("/", "abc", "def"))
  }

  "to seq with relative path" in {
    Path("abc/def").toSeq should be(Seq("abc", "def"))
  }
