package urwerk.io.http.client

import urwerk.io.http.{Headers, HttpStatusException, Request, Response}
import urwerk.io.{ByteString, http}
import urwerk.source.{Singleton, Source}

import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.ByteBuffer
import java.util.concurrent.Flow.Publisher
import java.util.{List => JList}

import scala.jdk.CollectionConverters._

trait Client:
  def request(request: Request): Singleton[Response]
  
  def acceptStatusCode(pred: Int => Boolean): Client
  
  def denyStatusCode(pred: Int => Boolean): Client
 
end Client

object Client: 
  def apply(): Client = 
    ClientImpl(DefaultStatusCodeAcceptPedicate, DefaultStatusCodeDenyPedicate)
    
  private val DefaultStatusCodeDenyPedicate = (statusCode: Int) => false

  private val DefaultStatusCodeAcceptPedicate = (statusCode: Int) => true

  given Client = apply()

end Client

private case class ClientImpl(acceptPredicate: Int => Boolean, denyPredicate: Int => Boolean) extends Client:

  def request(request: Request): Singleton[Response] = 
    val client = HttpClient.newBuilder()
      .build()

    val builder = HttpRequest.newBuilder()
      .method(request.method.name, BodyPublishers.noBody())
      .uri(request.uri)

    val req = request.headers.headers.foldLeft(builder){case (builder, (name, vals)) =>
        vals.foldLeft(builder)((builder, value) =>
          builder.header(name, value.toString))
      }
      .build()

    Singleton(req)
      .flatMap{req =>
        val respFuture = client.sendAsync(req, BodyHandlers.ofPublisher())
        Singleton.from(respFuture)
      }
      .map(mapResponse _)
      .flatMap(acceptHandler)
      .flatMap(denyHandler)

  def acceptStatusCode(pred: Int => Boolean): Client =
    copy(acceptPredicate = pred)

  def denyStatusCode(pred: Int => Boolean): Client =
    copy(denyPredicate = pred)

  private def acceptHandler(resp: Response): Singleton[Response] =
    if acceptPredicate(resp.statusCode) then Singleton(resp)
    else Singleton.error(new HttpStatusException(resp.statusCode))

  private def denyHandler(resp: Response): Singleton[Response] =
    if !denyPredicate(resp.statusCode) then Singleton(resp)
    else Singleton.error(new HttpStatusException(resp.statusCode))
    
  private def mapResponse(response: HttpResponse[Publisher[JList[ByteBuffer]]]): Response =
    val body = Source.from(response.body)
      .flatMap{buffers=>
        Source.from(buffers.asScala)}
      .map(ByteString.from(_))

    val headers = response.headers()
      .map.asScala.view
      .map((name, values) => (name, values.asScala.to(Seq)))
      .foldLeft(Headers()){case (headers, (name, vals)) =>
        headers.add(name, vals)}

    http.Response(statusCode = response.statusCode, headers = headers, body = body)

    
  