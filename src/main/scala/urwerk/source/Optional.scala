package urwerk.source

import _root_.reactor.core.publisher.Flux
import reactor.FluxConverters.*
import scala.jdk.OptionConverters.*
import urwerk.source.internal.FluxOptional

object Optional:

  def apply[A](elem: A): Optional[A] =
    wrap(Flux.just(elem))

  def apply[A](): Optional[A] =
    wrap(Flux.empty())

  def apply[A](elemOpt: Option[A]): Optional[A] = 
    elemOpt match
      case Some(elem) => apply(elem)
      case None => apply()

  def empty[A]: Optional[A] =
    wrap(Flux.empty())
      
  def error[A](error: Throwable): Optional[A] =
    wrap(Flux.error(error))

  private[source] def wrap[B](flux: Flux[B]): Optional[B] = 
    new Optional[B] {
      val fluxOptional = FluxOptional(flux)
      export fluxOptional.*
    }

end Optional

trait Optional[+A] extends Source[A]:
  def block: Option[A]

  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Optional[B]): Optional[B]

  def map[B](op: A => B): Optional[B]

end Optional
