package urwerk.source.reactor

import reactor.adapter.JdkFlowAdapter
import reactor.core.publisher.Flux
import urwerk.source.Source

import java.util.concurrent.Flow

object SourceConverters:
  extension [A](source: Source[A])
    def asFlux: Flux[A] =
      Source.unwrap(source)
