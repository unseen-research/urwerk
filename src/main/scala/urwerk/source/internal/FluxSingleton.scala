package urwerk.source.internal

import reactor.core.publisher.Flux
import urwerk.source.Source
import urwerk.source.Optional
import urwerk.source.reactor.SourceConverters.*
import urwerk.source.Singleton

class FluxSingleton[+A](flux: Flux[_<: A]) extends FluxSource[A](flux):
  def block: A =
    flux.blockFirst()
  
  override def filter(pred: A => Boolean): Optional[A] =
    Optional.wrap(flux.filter(pred(_)))

  override def filterNot(pred: A => Boolean): Optional[A] =
    filter(!pred(_))

  def flatMap[B](op: A => Singleton[B]): Singleton[B] =
    Singleton.wrap(
      flux.flatMap(elem =>
        op(elem).asFlux))
        
  override def map[B](op: A => B): Singleton[B] =
    Singleton.wrap(flux.map(op(_)))
  
