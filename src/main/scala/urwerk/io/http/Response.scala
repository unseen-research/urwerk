package urwerk.io.http

import urwerk.io.ByteString
import urwerk.source.Source

case class Response(statusCode: Int, headers: Headers, body: Source[ByteString])
