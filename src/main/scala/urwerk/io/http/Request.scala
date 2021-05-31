package urwerk.io.http

import urwerk.io.Uri
import urwerk.io.http.{Headers, Method}

case class Request(uri: Uri, method: Method, headers: Headers):
  def headers(op: Headers => Headers): Request = 
    copy(headers = op(headers))

object Request:
  object Get:
    def apply(uri: String): Request =
      apply(Uri(uri))
  
    def apply(uri: Uri): Request =
      Request(uri, Method.Get, Headers())