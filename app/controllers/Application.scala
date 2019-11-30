package controllers

import javax.inject.{Inject, Singleton}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.{Action, _}

import scala.concurrent.ExecutionContext

@Singleton
class Application @Inject()(ws: WSClient, cc: ControllerComponents)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action {
    Ok(views.html.index())
  }

  def proxy(url: String): Action[AnyContent] = Action.async { request: Request[AnyContent] =>
    val proxiedHeaders: Seq[(String,String)] =
      request.headers.toSimpleMap.
      filter {
        case (key: String, _: String) =>
          !Set(
            "Cookie", "Host", "Raw-Request-URI", "Referer",
            "Timeout-Access", "Tls-Session-Info", "User-Agent"
          ).contains(key)
      }.
      toSeq
    ws.
      url(s"${url}?${request.rawQueryString}").
      withHttpHeaders(proxiedHeaders: _*).
      stream().
      map { response: WSResponse =>
        val contentType = response.headers.get("Content-Type").flatMap(_.headOption)
          .getOrElse("application/octet-stream")
        val result = Status(response.status)

        response.headers.get("Content-Length") match {
          case Some(List(length)) =>
            result(response.body).as(contentType).withHeaders("Content-Length" -> length)
          case _ =>
            result.chunked(response.bodyAsSource).as(contentType)
        }
      }
  }
}
