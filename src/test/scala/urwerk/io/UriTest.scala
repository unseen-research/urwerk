package urwerk.io

import urwerk.test.TestBase
import urwerk.io.Uri.*

import java.net.URI

class UriTest extends TestBase:

  "absolute uri path" in {
    val path = Uri("http://host:77/abc/def").path
    path should be (Path("/abc/def"))
    path.absolute should be (true)
  }

  "absolute path uri" in {
    val path = Uri("/abc/def").path
    path should be (Path("/abc/def"))
    path.absolute should be (true)
  }

  "relative path uri" in {
    val path = Uri("abc/def").path
    path should be (Path("abc/def"))
    path.absolute should be(false)
  }

