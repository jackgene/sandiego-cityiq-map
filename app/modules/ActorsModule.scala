package modules

import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

import actors.CityIQAuthenticatingProxyActor

class ActorsModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor[CityIQAuthenticatingProxyActor]("cityiq-auth-proxy")
  }
}
