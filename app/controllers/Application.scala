package controllers

import actors.CityIQAuthenticatingProxy
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import javax.inject.{Inject, Named, Singleton}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.{Action, _}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class Application @Inject()
    (@Named("cityiq-auth-proxy") cityIqAuthProxy: ActorRef, ws: WSClient, cc: ControllerComponents)
    (implicit ec: ExecutionContext)
    extends AbstractController(cc) {
  private implicit val AskTimeout: Timeout = 30.seconds

  def index: Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  def proxy(url: String): Action[AnyContent] = Action.async { request: Request[AnyContent] =>
    val proxiedHeaders: Seq[(String,String)] =
      request.headers.toSimpleMap.
      filter {
        case (key: String, _: String) => key == "Predix-Zone-Id"
      }.
      toSeq
    (
      cityIqAuthProxy ? CityIQAuthenticatingProxy.ProxiedRequest(
        ws.url(s"${url}?${request.rawQueryString}").withHttpHeaders(proxiedHeaders: _*)
      )
    ).
    map {
      case response: WSResponse =>
        val contentType: Option[String] =
          response.headers.get("Content-Type").flatMap(_.headOption)
        val result: Result = Status(response.status)(response.body)
        contentType match {
          case Some(contentType: String) => result.as(contentType)
          case None => result
        }
    }
  }
}
