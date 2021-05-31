package urwerk.io.http.client

import com.github.tomakehurst.wiremock.client.WireMock.*
import urwerk.io.ByteString
import urwerk.io.http.Header.*
import urwerk.io.{ByteString, Uri}
import urwerk.io.http.{HttpStatusException, Method, WiremockServer}
import urwerk.io.http.Request.Get
import urwerk.source.Source
import urwerk.source.TestOps.*
import urwerk.test.TestBase

class HttpTest extends TestBase with WiremockServer:

  "get" in {
    httpServer.stubFor(get(s"/get/resource/")
      .willReturn(aResponse()
        .withBody("some body content")))
    
    val body = Http(Get(s"${serverUrl}/get/resource/"))
      .flatMap(_.body)
      .mkString.block

    body should be ("some body content")
  }
  
  "get status code" in {
    httpServer.stubFor(get(s"/get/resource/")
      .willReturn(aResponse()
        .withStatus(222)))

    val statusCode = Http(Get(s"${serverUrl}/get/resource/"))
      .map(_.statusCode)
      .block
    
    statusCode should be (222)
  }

  "status code interceptor" - {
    def stub(statusCode: Int) = httpServer.stubFor(get(s"/get/resource/")
      .willReturn(aResponse()
        .withStatus(statusCode)))

    "default accept handler accepts everything" in {
      stub(500)

      val statusCode = Http(Get(s"${serverUrl}/get/resource/"))
        .map(_.statusCode).block

      statusCode should be(500)
    }

    "deny with accept handler" in {
      stub(200)
      
      given (using client: Client): Client =
        client.acceptStatusCode(_ != 200)

      singletonProbe(
          Http(Get(s"${serverUrl}/get/resource/")))
        .expectErrorMatches(error =>
          error.isInstanceOf[HttpStatusException]
            && error.asInstanceOf[HttpStatusException].statusCode == 200)
        .verify()
    }

    "default deny handler accepts everything" in {
      stub(500)

      val statusCode = Http(Get(s"${serverUrl}/get/resource/"))
        .map(_.statusCode).block

      statusCode should be(500)
    }

    "deny with deny handler" in {
      stub(200)

      given (using client: Client): Client =
        client.denyStatusCode(_ == 200)

      singletonProbe(
          Http(Get(s"${serverUrl}/get/resource/")))
        .expectErrorMatches(error =>
          error.isInstanceOf[HttpStatusException]
            && error.asInstanceOf[HttpStatusException].statusCode == 200)
        .verify()
    }
  }

  "request headers" in {
    httpServer.stubFor(get("/get/resource/"))

    Http(Get(s"${serverUrl}/get/resource/")
      .headers(_.add[Accept]("xml", "json"))).block

    verify(getRequestedFor(urlEqualTo("/get/resource/"))
      .withHeader(valueOf[Accept], equalTo("xml"))
      .withHeader(valueOf[Accept], equalTo("json")))
  }

  "response headers" in {
    httpServer.stubFor(get(s"/get/resource/")
      .willReturn(aResponse()
        .withHeader("any-header", "any-val1", "any-val2")
        .withHeader("other-header", "other-val")))

    val headers = Http(Get(s"${serverUrl}/get/resource/"))
      .map(_.headers)
      .block

    headers["any-header"] should be (Seq("any-val1", "any-val2"))
    headers["other-header"] should be (Seq("other-val"))
  }
