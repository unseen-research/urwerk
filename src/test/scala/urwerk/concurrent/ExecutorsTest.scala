package urwerk.concurrent

import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService

import urwerk.test.TestBase

class ExecutorsTest extends TestBase:

  "execution context to executor service" in {
    val ec: ExecutionContext = ExecutionContext.global

    val es: ExecutionContextExecutorService = ec.toExecutorService
    val res = es.submit(() => "result").get(3, TimeUnit.SECONDS)

    res should be ("result")

  }
