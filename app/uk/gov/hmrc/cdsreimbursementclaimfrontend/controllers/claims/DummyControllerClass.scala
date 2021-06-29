/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.syntax.all._
import com.google.inject.Inject
import play.api.i18n.Messages.implicitMessagesProviderToMessages
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache2
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.DummyControllerClass.Repository
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.{Journey, SubmitPage, TemplateContent}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.Journey.{BulkJourney, SingleJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateContent._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.SubmitPage._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import java.util.UUID
import scala.util.Random

class DummyControllerClass @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache2,
  displayJourney: pages.display_journey
)(implicit viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def testSingle(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      val (messagesKey, submitUrl) = extractContent(SingleJourney())
      Ok(displayJourney(messagesKey, submitUrl))
    }

  def testBulk(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      val (messageKey, submitUrl) = extractContent(BulkJourney())
      Ok(displayJourney(messageKey, submitUrl))
    }

  def test(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      val (messageKey, submitUrl) = Repository.getRandom match {
        case singleJourney: SingleJourney => extractContent(singleJourney)
        case bulkJourney: BulkJourney     => extractContent(bulkJourney)
      }
      Ok(displayJourney(messageKey, submitUrl))
    }

  def testSubmit(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Repository
        .find(id)
        .map {
          case _: SingleJourney =>
            Redirect(implicitly[SubmitPage[SingleJourney]].nextUrl)
          case _: BulkJourney   =>
            Redirect(implicitly[SubmitPage[BulkJourney]].nextUrl)
        }
        .getOrElse(NotFound)
    }

  def extractContent[T <: Journey](journey: T)(implicit content: TemplateContent[T]): (String, Call) =
    (content.key, content.submitUrlFor(journey))

}

object DummyControllerClass {

  object Repository {

    private val rnd: Random = new Random()

    @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
    private val journeys: IndexedSeq[Journey] = IndexedSeq(SingleJourney(), BulkJourney())

    def getRandom: Journey = journeys(rnd.nextInt(journeys.length))

    def find(id: UUID): Option[Journey] = journeys.find(_.id === id)
  }
}
