package controllers

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, _}

@Singleton
class HomeController @Inject() (cc: ControllerComponents)
    extends AbstractController(cc) {
  def index: Action[AnyContent] = Action {
    Ok(views.html.index())
  }
}
