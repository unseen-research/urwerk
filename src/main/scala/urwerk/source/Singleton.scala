package urwerk.source

import _root_.reactor.core.publisher.{Flux, Mono}
import reactor.SourceConverters.*

import java.util.concurrent.CompletableFuture

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
    new Singleton[A] with SingletonOps[A] {
      private[source] val underlying: Flux[_ <: A] = flux
    }
  
end Singleton

trait Singleton[+A] extends Source[A]:

  def block: A
  
  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Singleton[B]): Singleton[B]
  
  def map[B](op: A => B): Singleton[B]

end Singleton

transparent trait SingletonOps[+A] extends SourceOps[A]:

  def block: A =
    underlying.blockFirst()
  
  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(underlying.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Singleton[B]): Singleton[B] =
    Singleton.wrap(
      underlying.flatMap(elem =>
        op(elem).asFlux))
        
  override def map[B](op: A => B): Singleton[B] =
    Singleton.wrap(underlying.map(op(_)))
  
end SingletonOps
