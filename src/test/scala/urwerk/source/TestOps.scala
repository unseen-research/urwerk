package urwerk.source

import _root_.reactor.test.StepVerifier
import _root_.reactor.test.StepVerifier.FirstStep
import org.reactivestreams.Publisher
import urwerk.source.reactor.FluxConverters.*

object TestOps:

  def singletonProbe[A](source: Singleton[A]): FirstStep[A] =
    sourceProbe(source)

  def optionalProbe[A](source: Optional[A]): FirstStep[A] =
    sourceProbe(source)

  def sourceProbe[A](source: Source[A]): FirstStep[A] =
    StepVerifier.create(source.toFlux)

  def sourceProbe[A](request: Long, source: Source[A]): FirstStep[A] =
    StepVerifier.create(source.toFlux, request)

  extension[A](source: Source[A])
    def toVerifier  = sourceProbe(source)

  extension[A](source: Singleton[A])
    def assertSingleton = source

  extension[A](source: Optional[A])
    def assertOptional = source
