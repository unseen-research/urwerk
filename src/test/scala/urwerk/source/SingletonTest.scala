package urwerk.source

import urwerk.source.TestOps.*
import urwerk.test.TestBase

import java.util.concurrent.CompletableFuture
import scala.concurrent.Future
import scala.util.Try
import java.io.IOException

class SingletonTest extends TestBase:

  "apply one element" in {
    singletonProbe(
        Singleton(7))
      .expectNext(7)
      .verifyComplete()
  }

  "block" in {
    Singleton(7).block should be (7)
  }

  "block with IOExcepion" in {
    intercept[IOException]{
      Singleton.error(IOException())
        .block
    }
  }

  "defer" in {
    val sources = Iterator(
      Singleton(7),
      Singleton.error(new IllegalArgumentException()),
      Singleton(4))

    val source = Singleton.defer(
      sources.next)

    singletonProbe(source)
      .expectNext(7)
      .verifyComplete()

    singletonProbe(source)
      .expectError(classOf[IllegalArgumentException])
      .verify()

    singletonProbe(source)
      .expectNext(4)
      .verifyComplete()
  }

  "error" in {
    singletonProbe(
        Singleton.error(IllegalArgumentException()))
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "filter true" in {
    optionalProbe(
        Singleton(42)
          .filter(_ == 42))
      .expectNext(42)
      .verifyComplete()
  }

  "filter false" in {
    optionalProbe(
        Singleton(42)
          .filter(_ != 42))
      .verifyComplete()
  }

  "filter not true" in {
    optionalProbe(
        Singleton(42)
          .filterNot(_ == 42))
      .verifyComplete()
  }

  "filter not false" in {
    optionalProbe(
        Singleton(42)
          .filterNot(_ != 42))
      .expectNext(42)
      .verifyComplete()
  }

  "flatMap with singleton" in {
    singletonProbe(
        Singleton(42)
          .flatMap(item => Singleton(item + 1)))
      .expectNext(43)
      .verifyComplete()
  }

  "flatMap with source" in {
    sourceProbe(
        Singleton(1)
          .flatMap(item => Source(s"a:$item", s"b:$item")))
      .expectNext("a:1", "b:1")
      .verifyComplete()
  }

  "from completable future" in {
    val value = Singleton.from(
        CompletableFuture.completedFuture(77))
      .block
    value should be (77)
  }

  "from successful future" in {
    val value = Singleton.from(
            Future.successful(77))
      .block
    value should be (77)
  }

  "from failed future" in {
    singletonProbe(
        Singleton.from(
          Future.failed(IllegalArgumentException())))
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "from successful try" in {
    val value = Singleton.from(
        Try(77))
      .block
    value should be (77)
  }

  "from failed try" in {
    singletonProbe(
        Singleton.from(
          Try(throw IllegalArgumentException())))
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }
  "map" in {
    singletonProbe(
        Singleton(1)
          .map(_.toString))
      .expectNext("1")
      .verifyComplete()
  }