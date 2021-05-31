package urwerk.io.http

import urwerk.io.http.Header
import urwerk.io.http.Header.{Accept, ApplySeq, ApplyVal, ApplyValAux, ContentLength, UnapplySeq, Val}
import urwerk.io.http.{Header, Headers}
import urwerk.test.TestBase

import scala.compiletime.constValue

class HeadersTest extends TestBase:

  "add header with singleton value" in {
    val headers = Headers()
      .add["int-header"](7)
      .add["int-header"](77)

    headers["int-header"] should be (Some(77))
  }

  "add header with seq value" in {
    val headers = Headers()
      .add["int-seq-header"](7, 77, 777)
      .add["int-seq-header"](7777)

    headers["int-seq-header"] should be (Seq(7, 77, 777, 7777))
  }

  "add header by name" in {
    val headers = Headers()
      .add("any-name", 7)
      .add("any-name", "seven", "7")
    
    headers("any-name") should be(Seq(7, "seven", "7"))
  }

  "set header with singleton value" in {
    val headers = Headers()
      .set["int-header"](7)

    headers["int-header"] should be (Some(7))
  }

  "set header with seq value" in {
    val headers = Headers()
      .set["int-seq-header"](7)
      .set["int-seq-header"](7, 77)
      .set["int-seq-header"](7, 77, 777)

    headers["int-seq-header"] should be (Seq(7, 77, 777))
  }

  "get header by singleton type using int extractor" in {
    val headers = Headers("int-header" -> "41", "int-header" -> "42")

    headers["int-header"] should be(Some(42))
  }

  "get header by singleton type using any extractor" in {
    val headers = Headers("any-name" -> 42, "another-name" -> "string-value", "any-name" -> "42")

    headers["any-name"] should be(Seq(42, "42"))

    type AnyHeader = "another-name"
    headers[AnyHeader] should be(Seq("string-value"))
    
    headers["not-existing"] should be (Seq())
  }

  "get header by name" in {
    val headers = Headers("any-name" -> 42, "another-name" -> 77, "any-name" -> "42")
    
    headers("any-name") should be(Seq(42, "42"))
    headers("other-name") should be (Seq())
  }

  "content-length header" in {
    val headers = Headers()
      .set[ContentLength](42L)

    headers[ContentLength] should be (Some(42))
  }

  "accept header" in {
    val headers = Headers()
      .set[Accept]("xml")
      .add[Accept]("txt", "csv")

    headers[Accept] should be (Seq("xml", "txt", "csv"))
  }

  given IntHeader: UnapplySeq["int-header"] with ApplyVal["int-header"] with {
    type V = Int
    type VV = Option[Int]
    def unapply(values: Seq[Val]): Option[Int] =
      values.lastOption
        .map{_ match
          case value: Number => value.intValue
          case value => value.toString.toInt }

    def apply(value: V): Val = value
  }

  given IntSeqHeader: UnapplySeq["int-seq-header"] with ApplyVal["int-seq-header"] with ApplySeq["int-seq-header"] with {
    type V = Int
    type VV = Seq[Int]
    def unapply(values: Seq[Val]): Seq[Int] = {
       values.map{_ match
          case value: Number => value.intValue
          case value => value.toString.toInt }
    }
    def apply(value: V): Val = value
    def apply(values: Seq[V]): Seq[Val] = values
  }
