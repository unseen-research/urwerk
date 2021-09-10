package urwerk.source.reactor

import reactor.adapter.JdkFlowAdapter
import reactor.core.publisher.Flux
import urwerk.source.Source

import java.util.concurrent.Flow
import urwerk.source.internal.FluxSource

object FluxConverters:
  extension [A](source: Source[A])
    def asFlux: Flux[A] =
      JdkFlowAdapter.flowPublisherToFlux(
        source.toPublisher)
