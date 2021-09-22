package urwerk.io

import urwerk.test.TestBase

class PathTest extends TestBase:
  val Absolute = true

  Seq(
    (Seq(""), Seq(), !Absolute),
    (Seq("abc"), Seq("abc"), !Absolute),
    (Seq("abc/def"), Seq("abc", "def"), !Absolute),

    (Seq("/"), Seq(), Absolute),
    (Seq("///abc//def//"), Seq("abc", "def"), Absolute),
    (Seq("/abc/def"), Seq("abc", "def"), Absolute),
  )
    .foreach{(givenElems, expectedElems, expectedAbsolute) =>
      val suffix = (givenElems).mkString(":")
      val path = Path(givenElems*)
      s"create path $suffix" in {
        path.elements should be(expectedElems)
      }

      s"is absolute $suffix" in {
        Path(givenElems*).absolute should be(expectedAbsolute)
      }
    }

  "make string" in {
    Path("/abc/def").mkString("(", ",", ")") should be("(abc,def)")
  }

  Seq(
      (Path(""), ""),
      (Path("/"), "/"),
      (Path("/abc"), "/abc"),
      (Path("abc/def"), "abc/def"))
    .foreach{(path, string) =>
      s"to string with $string" in {
        path.toString should be(string)
      }
    }

  "parent on empty path throws no such element excption" in {
    intercept[NoSuchElementException] {
      Path().parent
    }
  }

  "parent on root path throws no such element excption" in {
    intercept[NoSuchElementException] {
      Path("/").parent
    }
  }

  "parent on single element relative path throws no such element excption" in {
    intercept[NoSuchElementException] {
      Path("abc").parent
    }
  }

  "parent on single element absolute path returns root path" in {
    Path("/abc").parent should be (Path("/"))
  }

  "parent on absolute path" in {
    Path("/abc/def/ghi").parent should be (Path("/abc/def"))
  }

  "parent on relative path" in {
    Path("/abc/def/ghi").parent should be (Path("/abc/def"))
  }

  "parent option on empty path yields none" in {
    Path().parentOption should be(None)
  }

  "parent option on root path yields none" in {
      Path("/").parentOption should be(None)
  }

  "parent option on single element relative path yields none" in {
      Path("abc").parentOption should be(None)
  }

  "parent option on single element absolute path" in {
    Path("/abc").parentOption should be (Some(Path("/")))
  }

  "parent option absolute path" in {
    Path("/abc/def/ghi").parentOption should be (Some(Path("/abc/def")))
  }

  "parent option relative path" in {
    Path("abc/def/ghi").parentOption should be (Some(Path("abc/def")))
  }

  "resolve path with element" in {
    val path = Path("abc").resolve("def").resolve("ghi/jkl")
    path.elements should be (Seq("abc", "def", "ghi", "jkl"))
  }

  "resolve path with / operator" in {
    val path = Path("abc") / "def" / "ghi/jkl"
    path.elements should be (Seq("abc", "def", "ghi", "jkl"))
  }

  "resolve with relative other path" in {
    Path("abc/def").resolve(Path("123/456")) should be (Path("abc", "def", "123", "456"))
    Path("/abc/def").resolve(Path("123/456")) should be (Path("/", "abc", "def", "123", "456"))
  }

  "resolve with absolute other path" in {
    Path("abc/def").resolve(Path("/123/456")) should be (Path("/", "123", "456"))
    Path("/abc/def").resolve(Path("/123/456")) should be (Path("/", "123", "456"))
  }

  "toAbsolute from absolute path" in {
    Path("/abc/def").toAbsolute should be (Path("/abc/def"))
  }

  "toAbsolute from relative path" in {
    Path("abc/def").toAbsolute should be (Path("/abc/def"))
  }