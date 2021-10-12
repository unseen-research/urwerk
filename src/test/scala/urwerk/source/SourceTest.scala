package urwerk.source

import java.time.Duration
import java.util.concurrent.Flow
import java.util.concurrent.Flow.Subscriber
import java.util.concurrent.Flow.Subscription

import _root_.reactor.adapter.JdkFlowAdapter
import _root_.reactor.core.Exceptions
import _root_.reactor.core.publisher.Flux
import _root_.reactor.test.StepVerifier
import _root_.reactor.test.StepVerifierOptions
import _root_.reactor.test.publisher.TestPublisher

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}
import scala.util.Random

import urwerk.source.TestOps.*
import urwerk.test.TestBase
import urwerk.source.Signal.{Next, Complete, Error}
import urwerk.source.reactor.FluxConverters.*
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch

class SourceTest extends TestBase:
  "apply" in {
    Source(0, 8, 15)
      .toVerifier
      .expectNext(0, 8, 15)
      .verifyComplete()
  }

  "cache" in {
    val src = Source.create[Int](
        _.next(Random.nextInt).next(Random.nextInt).next(Random.nextInt).complete())
      .cache

    val r1 = src.toSeq.block
    val r2 = src.toSeq.block
    r1 should be (r2)
  }

  "concat sources" in {
    Source(Source("abc", "def"), Source("123", "456"))
      .concat[String]
      .toVerifier
      .expectNext("abc", "def", "123", "456")
      .verifyComplete()
  }

  "concat sources delay error" in {
    Source(Source.error(IllegalArgumentException()), Source("abc", "def"), Source("123", "456"))
      .concatDelayError
      .toVerifier
      .expectNext("abc", "def", "123", "456")
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "concat other source" in {
    Source("abc", "def")
      .concat(Source("123", "456"))
      .toVerifier
      .expectNext("abc", "def", "123", "456")
      .verifyComplete()
  }

  "create" in {
    Source.create[Int](
        _.next(1)
          .next(2)
          .next(3)
          .complete())
      .toVerifier
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "create with error" in {
    Source.create[Int](
        _.next(1)
          .next(2)
          .error(new IllegalArgumentException("message")))
      .toVerifier
      .expectNext(1, 2)
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "create with back pressure strategy" in {
    //TODO test the backpressure strategy
    Source.create[Int](BackPressureStrategy.Error)(
        _.next(1)
          .next(2)
          .error(new IllegalArgumentException("message")))
      .toVerifier
      .expectNext(1, 2)
      .expectError(classOf[IllegalArgumentException])
      .verify()


    // val tp = Source.create(BackPressureStrategy.Error){sink =>
    //   sink.next("A", "B", "C", "D")



    // }.onBackpressureBuffer(3, BufferOverflowStrategy.Error).toFlux


    // StepVerifier.create(tp, StepVerifierOptions.create()
    //     .initialRequest(0))
    //   .expectSubscription()
    //   .`then`(() => tp.next("A", "B", "C", "D"))
    //   .expectNoEvent(Duration.ofMillis(100))
    //   .thenRequest(3)
    //   .expectNext("A", "B", "C")
    //   .expectErrorMatches(Exceptions.isOverflow)
    //   .verify(Duration.ofSeconds(5))
    // sourceProbe(
    //     Source.create[Int](BackPressureStrategy.Error){sink =>
    //       sink.next(1)
    //         .next(2)
    //         .next(3)
    //         .complete()
    //     })
    //   .expectNext(1, 2, 3)
    //   .verifyComplete()
  }

  "defer" in {
    val sources = Iterator(
      Source(1, 2, 3),
      Source.error(new IllegalArgumentException()),
      Source(4, 5, 6))

    val source = Source.defer(
      sources.next)

    source.toVerifier
      .expectNext(1, 2, 3)
      .verifyComplete()

    source.toVerifier
      .expectError(classOf[IllegalArgumentException])
      .verify()

    source.toVerifier
      .expectNext(4, 5, 6)
      .verifyComplete()
  }

  "defer error" in {
    val errors = Iterator(
      new IllegalArgumentException(),
      new IllegalStateException())

    val source = Source.deferError(
      errors.next)

    source.toVerifier
      .expectError(classOf[IllegalArgumentException])
      .verify()

    source.toVerifier
      .expectError(classOf[IllegalStateException])
      .verify()
  }

  "distinct" in {
    Source(true, false, true, false, true, false).distinct
      .toVerifier
      .expectNext(true, false)
      .verifyComplete()
  }

  "do on complete" in {
    var completed: Boolean = false

    Source()
      .doOnComplete{completed = true}
      .subscribe()

    completed should be (true)
  }

  "do on error" in {
    var error: Throwable = new RuntimeException()

    Source.error(new IllegalArgumentException())
      .doOnError(error = _)
      .subscribe()

    error shouldBe a[IllegalArgumentException]
  }

  "do on next" in {
    val elems = ListBuffer[Int]()

    Source(1, 2, 3)
      .doOnNext(elem => elems += elem)
      .subscribe()

    elems should be(Seq(1, 2, 3))
  }

  "empty" in {
    Source.empty
      .toVerifier
      .verifyComplete()
  }

  "error" in {
    Source.error(new IllegalArgumentException())
      .toVerifier
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "filter" in {
    Source(1, 2, 3, 4)
      .filter(_ % 2 == 0)
      .toVerifier
      .expectNext(2, 4)
      .verifyComplete()
  }

  "filter not" in {
    Source(1, 2, 3, 4)
      .filterNot(_ % 2 == 0)
      .toVerifier
      .expectNext(1, 3)
      .verifyComplete()
  }

  "flatMap" in {
    Source(1, 2, 3)
      .flatMap(item => Source(s"a:$item", s"b:$item"))
      .toVerifier
      .expectNext("a:1", "b:1", "a:2", "b:2", "a:3", "b:3")
      .verifyComplete()
  }

  "flatMap with concurrency" in {
    Source(1, 2, 3)
      .flatMap(2)(item =>
        Source(s"first $item", s"second $item"))
      .toVerifier
      .expectNext("first 1", "second 1", "first 2", "second 2", "first 3", "second 3")
      .verifyComplete()
  }

  "flat map with concurrency and prefetch" in {
    Source(1, 2, 3).flatMap(2, 2)(item =>
        Source(s"first $item", s"second $item"))
      .toVerifier
      .expectNext("first 1", "second 1", "first 2", "second 2", "first 3", "second 3")
      .verifyComplete()
  }

  "fold" in {
    Source(1, 2).fold("0")((ctx, item) =>
        s"$ctx $item")
      .toVerifier
      .expectNext("0 1 2")
      .verifyComplete()
  }

  "from iterable" in {
    Source.from(Seq(1, 2, 3))
      .toVerifier
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "from publisher" in {
    val publisher: Flow.Publisher[Int] = Source(1, 2, 3).toPublisher

    Source.from(publisher)
      .toVerifier
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "head of empty source throws NoSuchElementException" in {
    Source().head
      .toSingletonVerifier
      .expectError(classOf[NoSuchElementException])
      .verify()
  }

  "head" in {
    Source(1, 2, 3).head
      .toSingletonVerifier
      .expectNext(1)
      .verifyComplete()
  }

  "head option of empty source" in {
    Source().headOption
      .toOptionalVerifier
      .verifyComplete()
  }

  "head option" in {
    Source(1, 2, 3).headOption
      .toOptionalVerifier
      .expectNext(1)
      .verifyComplete()
  }

  "last of empty source throws NoSuchElementException" in {
    Source().last
      .toSingletonVerifier
      .expectError(classOf[NoSuchElementException])
      .verify()
  }

  "last" in {
    Source(1, 2, 3).last
      .toSingletonVerifier
      .expectNext(3)
      .verifyComplete()
  }

  "last option of empty source" in {
    Source().lastOption
      .toOptionalVerifier
      .verifyComplete()
  }

  "last option" in {
    Source(1, 2, 3).lastOption
      .toOptionalVerifier
      .expectNext(3)
      .verifyComplete()
  }

  "last option transmit the error" in {
    Source.error(new UnsupportedOperationException())
      .lastOption
      .toOptionalVerifier
      .expectError(classOf[UnsupportedOperationException])
      .verify()
  }

  "map" in {
    val elems = Source(1, 2, 3)
      .map(_.toString).toSeq.block
    elems should be (Seq("1", "2", "3"))
  }

  "materialize" in {
    val elems = Source(1, 2, 3).materialize
      .toSeq.block
    elems should be (Seq(Next(1), Next(2), Next(3), Complete))
  }

  // "dematerialize" in {
  //   val elems = Source(1, 2, 3).materialize
  //     .dematerialize
  //     .toSeq.block
  //   elems should be (Seq(1, 2, 3))
  // }

  // "dematerialize with error" in {
  //   sourceProbe(
  //       Source.error(IllegalArgumentException()).materialize
  //         .dematerialize)
  //     .expectError(classOf[UnsupportedOperationException])
  //     .verify()
  // }

  "merge with other" in {
    val elems = Source(1, 2, 3).merge(
        Source("4", "5", "6"))
      .toSeq.block.toSet
    elems should be (Set(1, 2, 3, "4", "5", "6"))
  }

  "merge" in {
    sourceProbe(
        Source(Source("abc", "def"), Source("123", "456"))
          .merge)
      .expectNext("abc", "def", "123", "456")
      .verifyComplete()
  }

  "merge delay error with other" in {
    sourceProbe(
        Source.error[String](IllegalArgumentException())
          .mergeDelayError(7, Source("123", "456")))
      .expectNext("123", "456")
      .expectError(classOf[IllegalArgumentException])
      .verify()
  }

  "mkstring" in {
    Source(1, 2, 3).mkString.block should be("123")
  }

  "mkstring with separator" in {
    singletonProbe(
      Source(1, 2, 3)
        .mkString(", "))
      .expectNext("1, 2, 3")
      .verifyComplete()
  }

  "mkstring with start, separator, end" in {
    singletonProbe(
      Source(1, 2, 3)
        .mkString("> ", ", ", " <"))
      .expectNext("> 1, 2, 3 <")
      .verifyComplete()
  }

  "on backpressure buffer drop oldest" in {
    //TODO: test the buffer overflow strategy
    val elems = Source(1, 2, 3, 4, 5, 6, 7, 8)
        .onBackpressureBuffer(2, BufferOverflowStrategy.Error)
      .subscribe(new Subscriber{
        def onNext(elem: Int) = {
          //println(s"ONNEXT: $elem")
        }
        def onError(e: Throwable) = {
          //println(s"ONError $e")
        }
        def onComplete() = {
          //println("ONCOMPLETE")
        }
        def onSubscribe(s: Subscription) =
          s.request(2)
      })
  }

  "on error continue recover from error" in {
    var actualError: Throwable = RuntimeException()
    var actualElement: Any = 0

    sourceProbe(
        Source(1, 2, 3)
          .doOnNext(elem => if elem == 2 then throw IllegalArgumentException())
          .onErrorContinue{(error, elem) =>
            actualError = error
            actualElement = elem
          })
      .expectNext(1, 3)
      .verifyComplete()

    actualError shouldBe a [IllegalArgumentException]
    actualElement should be (2)
  }

  "on error continue pass error" in {
    sourceProbe(
        Source(1)
          .map(_ => throw IllegalArgumentException())
          .onErrorContinue((error, _) => throw (error)))
      .verifyError(classOf[IllegalArgumentException])
  }

  "on error map" in {
    sourceProbe(
        Source.error(IllegalArgumentException())
            .onErrorMap{case error: IllegalArgumentException =>
              IllegalStateException()})
      .verifyError(classOf[IllegalStateException])
  }

  "on error resume" in {
    sourceProbe(
        Source.error[Int](IllegalArgumentException())
          .onErrorResume(_ => Source(1, 2, 3)))
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "push" in {
    sourceProbe(
      Source.push[Int]{sink =>
        sink.next(1)
          .next(2)
          .next(3)
          .complete()
      })
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "reduce" in {
    sourceProbe(
      Source("1", "2", "3")
        .reduce {(acc, elem) => s"$acc $elem"})
    .expectNext("1 2 3")
    .verifyComplete()
  }

  "reduce single element" in {
    sourceProbe(
        Source("1")
          .reduce {(acc, elem) => throw new RuntimeException()})
      .expectNext("1")
      .verifyComplete()
  }

  "scan" in {
    sourceProbe(
        Source(1, 2).scan("0") { (ctx, item) =>
          s"$ctx $item"
        })
      .expectNext("0", "0 1", "0 1 2")
      .verifyComplete()
  }

  "scanWith" in {
    var start = "-1"
    val src = Source(1, 2).scanWith(start){ (ctx, item) =>
      s"$ctx $item"}

    start = "0"

    sourceProbe(src)
      .expectNext("0", "0 1", "0 1 2")
      .verifyComplete()
  }

  "subscribe" in {
    val elems = ListBuffer[String]()
    Source(1, 2, 3)
      .doOnNext(elem => elems += elem.toString)
      .doOnComplete(elems += "completed")
      .subscribe()

    elems should be(Seq("1", "2", "3", "completed"))
  }

  "subscribe close" in {
    var onCancel = false
    var onDispose = false
    var onRequest = 0L

    val closable = Source.create[Int]{ sink =>
        sink.onCancel{onCancel = true}
          .onDispose{onDispose = true}
          .onRequest(count => onRequest = count)
      }
      .subscribe()

    closable.close()

    onCancel should be (true)
    onDispose should be (true)
    onRequest should be (Long.MaxValue)
  }

  "subscribe with Flow.Subscriber" in {
    val items = ListBuffer[String]()

    Source(1, 2, 3).subscribe(new Flow.Subscriber[Int](){
      def onSubscribe(subscription: Flow.Subscription)={
        subscription.request(3)
        items += "onSubscribe"}
      def onNext(item: Int) = {
        items += item.toString}
      def onComplete() = {
        items += "onComplete"}
      def onError(throwable: Throwable): Unit = ???
    })

    items should be(Seq("onSubscribe", "1", "2", "3", "onComplete"))
  }

  "subscribe with onNext, onError, onComplete" in {
    val items = ListBuffer[String]()

    Source(1, 2, 3).subscribe(onNext =
      items += _.toString,
      onError = error => {},
      onComplete =
        items += "onComplete")

    items should be(Seq("1", "2", "3", "onComplete"))
  }

  "subscribe on execution context" in {
    val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
    ec.execute(() => Thread.currentThread.setName("test thread name"))

    val src = Source.create[String]{sink =>
        val name = Thread.currentThread.getName
        sink.next(name)
        sink.complete()
      }
      .subscribeOn(ec)

    src.last.block should be ("test thread name")
  }

  "subscribe on execution context with requestOnSeparateThread option" in {
    val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
    ec.execute(() => Thread.currentThread.setName("test thread name"))

    val src = Source.create[String]{sink =>
        val name = Thread.currentThread.getName
        sink.next(name)
        sink.complete()
      }
      .subscribeOn(ec, true)

    src.last.block should be ("test thread name")
  }

  "take until" in {
    sourceProbe(Source(0, 1, 2, 3, 4)
        .takeUntil(_ == 3))
      .expectNext(0, 1, 2, 3)
      .verifyComplete()
  }

  "take while" in {
    sourceProbe(Source(0, 1, 2, 3, 4)
        .takeWhile(_ < 4))
      .expectNext(0, 1, 2, 3)
      .verifyComplete()
  }

  "to sequence" in {
    val seq = Seq(1, 2, 3)
    singletonProbe(
      Source.from(seq).toSeq)
      .expectNext(seq)
      .verifyComplete()
  }

  "to sequence for empty source" in {
    singletonProbe(
      Source().toSeq)
      .expectNext(Seq())
      .verifyComplete()
  }

  "unfold" in {
    var state = 0
    val src = Source.unfold(state){state =>
      if(state<0) None
      else Some(s"state $state", state-1)}

    state = 3
    sourceProbe(src)
      .expectNext("state 3", "state 2", "state 1", "state 0")
      .verifyComplete()
  }

  "unfold with error" in {
    sourceProbe(
        Source.unfold(3){state =>
          throw new UnsupportedOperationException()
        })
      .verifyError(classOf[UnsupportedOperationException])
  }

  "unfold with doOnLastState" in {
    var state = 0
    var finalState = 7
    val src = Source.unfold(state, (state: Int) => finalState = state){state =>
      if(state<0) None
      else Some(s"state $state", state-1)}

    state = 3
    sourceProbe(src)
      .expectNext("state 3", "state 2", "state 1", "state 0")
      .verifyComplete()

    finalState should be (-1)
  }

  "unfold with doOnLastState with error" in {
    var finalState = 7
    sourceProbe(
        Source.unfold(3, (state: Int) => finalState = state){state =>
          throw new UnsupportedOperationException()
        })
      .verifyError(classOf[UnsupportedOperationException])
    finalState should be(3)
  }

  "to publisher" in {
    val publisher = Source(1, 2, 3).toPublisher
    val flux = JdkFlowAdapter.flowPublisherToFlux(publisher)

    StepVerifier.create(flux)
      .expectNext(1, 2, 3)
      .verifyComplete()
  }

  "using" in {
    val resources = Seq("A", "B").iterator

    var disposeRes = ""
    val src = Source.using(resources.next, res => disposeRes = res){res => Source(res)}

    src.last.block should be ("A")
    disposeRes should be ("A")

    src.last.block should be ("B")
    disposeRes should be ("B")
  }