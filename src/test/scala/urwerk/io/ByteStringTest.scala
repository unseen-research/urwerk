package urwerk.io

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import urwerk.io.ByteString.*
import urwerk.source.Source
import urwerk.test.TestBase
import scala.io.Codec

class ByteStringTest extends TestBase:
  "empty" - {
    "empty yields isEmpty true" in {
      ByteString.empty.isEmpty should be(true)
    }
    "empty yields noneEmpty false" in {
      ByteString.empty.nonEmpty should be(false)
    }
    "has zero size" in {
      ByteString.empty.size should be(0)
    }
    "is instance of ByteString" in {
      ByteString.empty shouldBe an[ByteString]
    }
    "no args apply is empty" in {
      ByteString().isEmpty should be(true)
    }
    "none empty yields isEmpty false" in {
      ByteString(1.toByte).nonEmpty should be(true)
    }
    "none empty yields nonEmpty true" in {
      ByteString(1.toByte).nonEmpty should be(true)
    }
  }

  "apply" - {
    "empty array" in {
      ByteString(Array[Byte]()).isEmpty should be(true)
    }
    "array" in {
      val bs = ByteString(Array[Byte](1, 2, 3))

      bs.array should be(array(1, 2, 3))
      bs.offset should be(0)
      bs.length should be(3)
    }
    "array is safe" in {
      val a = array(1, 2, 3)
      val bs = ByteString(a)
      a.update(1, -1)

      bs should be(ByteString.fromInt(1, 2, 3))
    }
    "empty iterable" in {
      val bs = ByteString(Seq())

      bs.array should be(array())
      bs.offset should be(0)
      bs.length should be(0)
    }
    "non empty iterable" in {
      val bs = ByteString(Seq[Byte](1, 2, 3))

      bs.array should be(array(1, 2, 3))
      bs.offset should be(0)
      bs.length should be(3)
    }
    "single byte" in {
      ByteString(1.toByte).toArray should be(Array[Byte](1))
    }
  }

  "create from" - {
    "single int with empty varargs" in {
      ByteString.fromInt(1).toArray should be(Array[Byte](1))
    }
    "int varargs" in {
      ByteString(1, 2, 3).toArray should be(Array[Byte](1, 2, 3))
    }
    "string" in {
      ByteString.from("greetings") should be(ByteString("greetings".getBytes))
    }
    "string with encoding" in {
      ByteString.from("greetings", UTF_8) should be(ByteString("greetings".getBytes(UTF_8)))
    }
    "empty buffer" in {
      val bs = ByteString.from(ByteBuffer.allocate(100).flip())
      bs should be(ByteString.empty)
    }
    "buffer" in {
      val buffer = ByteBuffer.allocate(100)
        .put(Array.tabulate[Byte](42) { _.toByte })
        .flip()
        .position(10)

      val bs = ByteString.from(buffer)
      bs should be(ByteString(Array.tabulate[Byte](42) { _.toByte }))
    }
    "remaining buffer" in {
      val buffer = ByteBuffer.allocate(100)
        .put(Array.tabulate[Byte](42) { _.toByte })
        .flip()
        .position(10)

      val bs = ByteString.fromRemaining(buffer)
      bs should be(ByteString(Array.tabulate[Byte](32) { index => (index + 10).toByte }))
    }
    "remaining empty buffer" in {
      val bs = ByteString.fromRemaining(ByteBuffer.allocate(100).flip())
      bs should be(ByteString.empty)
    }
  }

  "wrap" - {
    "array" in {
      ByteString.unsafeWrap(Array[Byte](1, 2, 3)) should be(ByteString(1, 2, 3))
    }
    "array is unsafe" in {
      val a = Array[Byte](1, 2, 3)
      val bs = ByteString.unsafeWrap(a)

      a.update(1, 0)
      bs should be(ByteString(1, 0, 3))
    }
    "array segment" in {
      val bs = ByteString.unsafeWrap(array(1, 2, 3, 4, 5, 6, 7), 2, 3)
      bs should be(ByteString(3, 4, 5))
    }
    "array segment is unsafe" in {
      val a = array(1, 2, 3, 4, 5, 6, 7)
      val bs = ByteString.unsafeWrap(a, 2, 3)

      a.update(1, 0)
      a.update(2, -7)
      bs should be(ByteString(-7, 4, 5))
    }
    "buffer" in {
      val buffer = ByteBuffer.allocate(100)
        .put(Array.tabulate[Byte](42) { _.toByte })
        .flip().position(10)

      ByteString.unsafeWrap(buffer) should be(ByteString(Array.tabulate[Byte](42) { _.toByte }))
    }
    "buffer fail if has no array" in {
      intercept[UnsupportedOperationException] {
        ByteString.unsafeWrap(ByteBuffer.allocate(100).asReadOnlyBuffer())
      }
    }
    "remaining buffer fail if has no array" in {
      intercept[UnsupportedOperationException] {
        ByteString.unsafeWrap(ByteBuffer.allocate(100).asReadOnlyBuffer())
      }
    }
    "fail with negative offset" in {
      intercept[IllegalArgumentException] {
        ByteString.unsafeWrap(array(1, 2), -1, 5)
      }
    }
    "fail with negative length" in {
      intercept[IllegalArgumentException] {
        ByteString.unsafeWrap(array(1, 2), -1, 5)
      }
    }
    "fail with offset exeeded upper bound" in {
      intercept[IndexOutOfBoundsException] {
        ByteString.unsafeWrap(array(1, 2), 3, 5)
      }
    }
    "fail with lenth exeeded upper bound" in {
      intercept[IndexOutOfBoundsException] {
        ByteString.unsafeWrap(array(1, 2), 0, 5)
      }
    }
  }

  "equal ops" - {
    "equal empty" in {
      ByteString() should equal(ByteString())
    }
    "equal non empty" in {
      ByteString(1, 2) should equal(ByteString(1, 2))
    }
    "not equal" in {
      ByteString(1, 2) should not equal (ByteString(1, 3))
    }
  }

  "invariant collection type operation" - {
    "++" in {
      val bs: ByteString = ByteString(1, 2, 3) ++ Seq[Byte](4, 5, 6)
      bs should be(ByteString(1, 2, 3, 4, 5, 6))
    }
    "concat" in {
      val bs: ByteString = ByteString(1, 2, 3).concat(Seq[Byte](4, 5, 6))
      bs should be(ByteString(1, 2, 3, 4, 5, 6))
    }
    "appendedAll" in {
      val bs: ByteString = ByteString(1, 2, 3).appendedAll(Seq[Byte](4, 5, 6))
      bs should be(ByteString(1, 2, 3, 4, 5, 6))
    }
    ":+" in {
      val bs: ByteString = ByteString(1, 2, 3) :+ (4)
      bs should be(ByteString(1, 2, 3, 4))
    }
    "appended" in {
      val bs: ByteString = ByteString(1, 2, 3) :+ (4)
      bs should be(ByteString(1, 2, 3, 4))
    }
    "+:" in {
      val bs: ByteString = 0 +: ByteString(1, 2, 3)
      bs should be(ByteString(0, 1, 2, 3))
    }
    "prepended" in {
      val bs: ByteString = ByteString(1, 2, 3).prepended(0)
      bs should be(ByteString(0, 1, 2, 3))
    }
    "++:" in {
      val bs: ByteString = Seq[Byte](-1, 0) ++: ByteString(1, 2, 3)
      bs should be(ByteString(-1, 0, 1, 2, 3))
    }
    "prependedAll" in {
      val bs: ByteString = ByteString(1, 2, 3).prependedAll(Seq[Byte](-1, 0))
      bs should be(ByteString(-1, 0, 1, 2, 3))
    }
    "take" in {
      val bs: ByteString = ByteString(1, 2, 3).take(2)
      bs should be(ByteString(1, 2))
    }
    "drop" in {
      val bs: ByteString = ByteString(1, 2, 3).drop(1)
      bs should be(ByteString(2, 3))
    }
    "filter" in {
      val bs: ByteString = ByteString(1, 2, 3).filter(_ == 1)
      bs should be(ByteString.fromInt(1))
    }
  }
  "to array" in {
    ByteString(1, 2).toArray should be(Array(1, 2))
  }
  "to array preserves immutablility" in {
    val bs = ByteString(1, 2)
    val a = bs.toArray

    a.update(0, 55)
    a should be(Array(55, 2))
    bs should be(ByteString(1, 2))
  }
  "to seq" in {
    ByteString(1, 2).toSeq should be(Seq[Byte](1, 2))
  }
  "to string" in {
    val byteString = ByteString.unsafeWrap(
      Array[Byte](1, 2, 3) ++ Array.from[Byte]("abc".getBytes) ++ Array[Byte](4, 5, 6),
      3, 3)
    byteString.toString should be ("abc")
  }
  "to string with char encoding" in {
    val byteString = ByteString.unsafeWrap(
      Array[Byte](1, 2, 3) ++ Array.from[Byte]("abc".getBytes) ++ Array[Byte](4, 5, 6),
      3, 3)
    byteString.toString(using Codec.UTF8) should be ("abc")
  }

  private def array(bytes: Byte*) = Array[Byte](bytes: _*)

end ByteStringTest
