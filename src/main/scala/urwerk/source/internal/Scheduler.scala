package urwerk.source.internal

import java.util.concurrent.TimeUnit

import reactor.core.scheduler.Scheduler.Worker as InnerWorker
import reactor.core.scheduler.Scheduler as InnerScheduler

import scala.jdk.FunctionConverters.given

import urwerk.source.Disposable
import urwerk.source.Scheduler
import urwerk.source.Scheduler.Worker
import java.time.Duration

class WorkerWrapper(inner: InnerWorker) extends Worker:
  export inner.{dispose, isDisposed}

  def schedule(task: () => Unit): Disposable =
    DisposableWrapper.wrap(inner.schedule(() => task()))

  def schedule(delay: Duration)(task: () => Unit): Disposable =
    DisposableWrapper.wrap(
      inner.schedule(() => task(), delay.toNanos, TimeUnit.NANOSECONDS))

  def schedule(initialDelay: Duration, period: Duration)(task: () => Unit): Disposable =
    DisposableWrapper.wrap(
      inner.schedulePeriodically(() => task(), initialDelay.toNanos, period.toNanos, TimeUnit.NANOSECONDS))

object SchedulerWrapper:
  def unapply(scheduler: Scheduler): Option[InnerScheduler] =
    if scheduler.isInstanceOf[SchedulerWrapper] then
      Some(scheduler.asInstanceOf[SchedulerWrapper].inner)
    else
      None

  def unwrap(scheduler: Scheduler): InnerScheduler =
    if scheduler.isInstanceOf[SchedulerWrapper] then
      scheduler.asInstanceOf[SchedulerWrapper].inner
    else
      throw UnsupportedOperationException()

class SchedulerWrapper(val inner: InnerScheduler) extends Scheduler:
  export inner.{dispose, isDisposed, start}

  def createWorker(): Worker =
    WorkerWrapper(inner.createWorker())

  def now(unit: TimeUnit): Long = inner.now(unit)

  def schedule(task: () => Unit): Disposable =
    DisposableWrapper.wrap(inner.schedule(() => task()))

  def schedule(delay: Duration)(task: () => Unit): Disposable =
    DisposableWrapper.wrap(
      inner.schedule(() => task(), delay.toNanos, TimeUnit.NANOSECONDS))

  def schedule(initialDelay: Duration, period: Duration)(task: () => Unit): Disposable =
    DisposableWrapper.wrap(
      inner.schedulePeriodically(() => task(), initialDelay.toNanos, period.toNanos, TimeUnit.NANOSECONDS))
