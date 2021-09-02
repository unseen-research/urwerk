package urwerk.source.internal

import reactor.core.publisher.Flux
import urwerk.source.Optional
import urwerk.source.reactor.FluxConverters.*
import scala.jdk.OptionConverters.*

class FluxOptional[+A](flux: Flux[_<: A]) extends FluxSource[A](flux):
  def block: Option[A] =
    flux.next.blockOptional.toScala
    
  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(flux.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Optional[B]): Optional[B] =
    Optional.wrap(
      flux.flatMap(elem => 
        op(elem).asFlux))
  
  override def map[B](op: A => B): Optional[B] =
    Optional.wrap(flux.map(op(_)))

