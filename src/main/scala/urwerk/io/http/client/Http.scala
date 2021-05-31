package urwerk.io.http.client

import urwerk.io.ByteString
import urwerk.io.http.{HttpStatusException, Request, Response}
import urwerk.source.Singleton

import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.ByteBuffer
import java.util.concurrent.Flow.Publisher
import java.util.{List => JList}
import scala.annotation.targetName
import scala.concurrent.duration
import scala.jdk.CollectionConverters._
import scala.util.Try

object Http :
  @targetName("request")
  def apply(request: Request)(using client: Client): Singleton[Response] =
    client.request(request)

end Http
