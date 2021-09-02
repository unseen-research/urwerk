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
    StepVerifier.create(source.asFlux)
