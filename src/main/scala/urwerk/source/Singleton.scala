package urwerk.source

import java.util.concurrent.CompletableFuture
import urwerk.source.internal.FluxSingleton

object Singleton extends SingletonFactory:
  export FluxSingleton.*

trait Singleton[+A] extends Source[A]:
  def block: A

  def filter(pred: A => Boolean): Optional[A]

  def filterNot(pred: A => Boolean): Optional[A]

  def flatMap[B](op: A => Singleton[B]): Singleton[B]

  def map[B](op: A => B): Singleton[B]

trait SingletonFactory:
  def apply[A](elem: A): Singleton[A]

  def defer[A](op: => Singleton[A]): Singleton[A]

  def error[A](error: Throwable): Singleton[A]

  def from[A](future: CompletableFuture[A]): Singleton[A]
