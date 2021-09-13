package urwerk.source

import urwerk.source.TestOps.*
import urwerk.test.TestBase

class OptionalTest extends TestBase:

  "apply one element" in {
    optionalProbe(
        Optional(7))
      .expectNext(7)
      .verifyComplete()
  }

  "apply no element" in {
    optionalProbe(
        Optional())
      .verifyComplete()
  }

  "apply some element" in {
    optionalProbe(
        Optional(Some(7)))
      .expectNext(7)
      .verifyComplete()
  }

  "apply none element" in {
    optionalProbe(
        Optional(None))
      .verifyComplete()
  }

  "block some element" in {
    Optional(1).block should be(Some(1))
  }

  "block none" in {
    Optional().block should be(None)
  }

  "empty" in {
    optionalProbe(
        Optional.empty[Int])
      .verifyComplete()
  }

  "error" in {
    optionalProbe(
        Optional.error(IllegalArgumentException()))
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "filter true" in {
    optionalProbe(
        Optional(42)
         .filter(_ == 42))
      .expectNext(42)
      .verifyComplete()
  }

  "filter false" in {
    optionalProbe(
        Optional(42)
          .filter(_ != 42))
      .verifyComplete()
  }

  "filter not true" in {
    optionalProbe(
        Optional(42)
          .filterNot(_ == 42))
      .verifyComplete()
  }

  "filter not false" in {
    optionalProbe(
        Optional(42)
          .filterNot(_ != 42))
      .expectNext(42)
      .verifyComplete()
  }

  "flatMap with optional" in {
    optionalProbe(
        Optional(42)
          .flatMap(item => Optional(item + 1)))
      .expectNext(43)
      .verifyComplete()
  }

  "flatMap with source" in {
    sourceProbe(
        Optional(1)
          .flatMap(item => Source(s"a:$item", s"b:$item")))
      .expectNext("a:1", "b:1")
      .verifyComplete()
  }

  "map" in {
    optionalProbe(
      Optional(1)
        .map(_.toString))
      .expectNext("1")
      .verifyComplete()
  }