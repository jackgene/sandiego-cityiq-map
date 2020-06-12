package actors

import akka.actor.{Actor, ActorLogging, Stash}
import akka.pattern.pipe
import javax.inject.Inject
import play.api.http.Status
import play.api.libs.json.JsValue
import play.api.libs.ws.{WSAuthScheme, WSClient, WSRequest, WSResponse}

import scala.concurrent.{ExecutionContext, Future}

object CityIQAuthenticatingProxyActor {
  private val TokenUrl = "https://auth.aa.cityiq.io/oauth/token?grant_type=client_credentials"
  // TODO Make configurable, or scrape from
  // https://www.sandiego.gov/sustainability/energy-and-water-efficiency/programs-projects/smart-city
  private val BasicAuthUsername = "PublicAccess"
  private val BasicAuthPassword = "qPKIadEsoHjyh226Snz7"

  case class ProxiedRequest(req: WSRequest)
  case class ProxiedResponse(res: WSResponse)
  case object AuthenticationFailed
}
class CityIQAuthenticatingProxyActor @Inject()(ws: WSClient)
    extends Actor with ActorLogging with Stash {
  import CityIQAuthenticatingProxyActor._

  private implicit val ec: ExecutionContext = context.dispatcher

  private def authenticate(): Unit = {
    context.become(authenticating)
    ws.url(TokenUrl).
      withAuth(BasicAuthUsername, BasicAuthPassword, WSAuthScheme.BASIC).
      get().
      pipeTo(self)
  }
  authenticate()

  private lazy val authenticating: Receive = {
    case okResponse: WSResponse if okResponse.status == Status.OK =>
      (okResponse.json \ "access_token").toOption match {
        case Some(accessTokenJs: JsValue) =>
          context.become(authenticated(accessTokenJs.as[String]))

        case None =>
          context.become(authenticationFailed)
      }
      unstashAll()

    case _: WSResponse =>
      context.become(authenticationFailed)
      unstashAll()

    case _: ProxiedRequest =>
      stash()
  }

  private def authenticated(accessToken: String): Receive = {
    case ProxiedRequest(req: WSRequest) =>
      val respFut: Future[WSResponse] =
        req.addHttpHeaders("Authorization" -> s"Bearer ${accessToken}").get()
      respFut.foreach { resp: WSResponse =>
        if (resp.status == Status.UNAUTHORIZED) authenticate()
      }
      respFut.map(ProxiedResponse).pipeTo(sender())
  }

  private val authenticationFailed: Receive = {
    case _ => sender() ! AuthenticationFailed
  }

  override def receive: Receive = PartialFunction.empty
}
