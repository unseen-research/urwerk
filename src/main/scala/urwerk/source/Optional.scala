package urwerk.source

import _root_.reactor.core.publisher.Flux
import urwerk.source.Source.wrap
import reactor.SourceConverters.*
import scala.jdk.OptionConverters.*

object Optional:

  def apply[A](elem: A): Optional[A] =
    wrap(Flux.just(elem))

  def apply[A](): Optional[A] =
    wrap(Flux.empty())

  def apply[A](elemOpt: Option[A]): Optional[A] = 
    elemOpt match
      case Some(elem) => apply(elem)
      case None => apply()
  
  def error[A](error: Throwable): Optional[A] =
    wrap(Flux.error(error))

  private[source] def wrap[B](flux: Flux[B]): Optional[B] = 
    new Optional[B] with OptionalOps[B] {
      private[source] val underlying = flux.singleOrEmpty().flux
    }

end Optional

trait Optional[+A] extends Source[A]:
  def block: Option[A]

  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Optional[B]): Optional[B]

  def map[B](op: A => B): Optional[B]

end Optional

transparent trait OptionalOps[+A] extends SourceOps[A]:

  def block: Option[A] =
    underlying.next.blockOptional.toScala
    
  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(underlying.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Optional[B]): Optional[B] =
    Optional.wrap(
      underlying.flatMap(elem => 
        op(elem).asFlux))
  
  override def map[B](op: A => B): Optional[B] =
    Optional.wrap(underlying.map(op(_)))

end OptionalOps
