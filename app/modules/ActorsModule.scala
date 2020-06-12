package modules

import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

import actors.CityIQAuthenticatingProxy

class ActorsModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor[CityIQAuthenticatingProxy]("cityiq-auth-proxy")
  }
}
