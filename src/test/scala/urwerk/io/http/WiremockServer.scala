package urwerk.io.http

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.options
import org.scalatest.TestSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

trait WiremockServer extends BeforeAndAfterAll, BeforeAndAfterEach:
  this: TestSuite =>

  val httpHost = WiremockServer.httpHost
  
  val httpServer = WiremockServer.httpServer

  val httpPort = WiremockServer.httpPort

  WireMock.configureFor(httpHost, httpPort)
  
  val serverUrl = s"http://$httpHost:$httpPort"

  override def beforeEach(): Unit =
    httpServer.resetAll()

object WiremockServer:

  val httpHost = "localhost"

  val httpServer = new WireMockServer(
    options.dynamicPort()
      .bindAddress(httpHost))

  httpServer.start()

  val httpPort = httpServer.port()

  WireMock.configureFor(httpHost, httpPort)

  val serverUrl = s"http://$httpHost:$httpPort"

  scala.sys.addShutdownHook(
    httpServer.stop())