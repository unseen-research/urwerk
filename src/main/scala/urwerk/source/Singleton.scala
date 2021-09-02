package urwerk.source

import _root_.reactor.core.publisher.{Flux, Mono}
import reactor.FluxConverters.*

import java.util.concurrent.CompletableFuture
import urwerk.source.internal.FluxSingleton

object Singleton:

  def apply[A](elem: A): Singleton[A] =
    wrap(Flux.just(elem))

  def defer[A](op: => Singleton[A]): Singleton[A] =
    wrap(Flux.defer(() =>
      op.asFlux))

  def error[A](error: Throwable): Singleton[A] =
    wrap(Flux.error(error))
    
  def from[A](future: CompletableFuture[A]): Singleton[A] =
    wrap(
      Mono.fromFuture(future)
        .flux())
        
  private[source] def wrap[A](flux: Flux[A]): Singleton[A] =
    new Singleton[A]{
      val fluxSingleton = new FluxSingleton(flux)
      export fluxSingleton.*
    }
  
end Singleton

trait Singleton[+A] extends Source[A]:

  def block: A
  
  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Singleton[B]): Singleton[B]
  
  def map[B](op: A => B): Singleton[B]

end Singleton
