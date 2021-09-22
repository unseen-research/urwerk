package urwerk.source


import java.util.concurrent.Executor
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit

import _root_.reactor.core.scheduler.Schedulers

import scala.concurrent.duration.Duration

object Scheduler:
  def fromExecutorService(executorService: ExecutorService): Scheduler =
    internal.SchedulerWrapper(Schedulers.fromExecutorService(executorService))

  def fromExecutor(executor: Executor): Scheduler =
    internal.SchedulerWrapper(Schedulers.fromExecutor(executor))

  trait Worker extends Disposable:
    def schedule(task: () => Unit): Disposable

    def schedule(delay: Duration)(task: () => Unit): Disposable

    def schedule(initialDelay: Duration, period: Duration)(task: () => Unit): Disposable


trait Scheduler extends Disposable:
  def createWorker(): Scheduler.Worker

  def now(unit: TimeUnit): Long

  def schedule(task: () => Unit): Disposable

  def schedule(delay: Duration)(task: () => Unit): Disposable

  def schedule(initialDelay: Duration, period: Duration)(task: () => Unit): Disposable

  def start(): Unit