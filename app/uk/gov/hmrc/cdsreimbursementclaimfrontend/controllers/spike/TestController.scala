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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.spike

import cats.implicits.catsSyntaxOptionId
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ContactName, Eori, SignedInUserDetails, UserType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.JourneySubmitStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.Session.GovernmentGatewaySession
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model.DraftClaim.{EntryNumberDraftClaim, MrnDraftClaim}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model.ReferenceNumber.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model.{Journey, ReferenceNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID

@Singleton
class TestController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def test =
    authenticatedActionWithSessionData { implicit request =>
      val sessions = Seq(
        GovernmentGatewaySession(
          GGCredId("6145079961943419"),
          UserType.Individual.some,
          SignedInUserDetails(
            Email("user@test.com").some,
            Eori("GB000000000000001"),
            Email("user@test.com"),
            ContactName("USER")
          ),
          Journey.singleFor(MrnDraftClaim(UUID.randomUUID())),
          JourneySubmitStatus.UnSubmitted
        ),
        GovernmentGatewaySession(
          GGCredId("6145079961943418"),
          UserType.Individual.some,
          SignedInUserDetails(
            Email("user@test.com").some,
            Eori("GB000000000000001"),
            Email("user@test.com"),
            ContactName("USER")
          ),
          Journey.scheduledFor(EntryNumberDraftClaim(UUID.randomUUID())),
          JourneySubmitStatus.UnSubmitted
        )
      )

      val journeys = sessions.map(session => nameJourney(session.journey))

      Ok("dddd")
    }

  def nameJourney[T <: ReferenceNumber](journey: Journey[T]): Unit = {

  }
}

object TestController {

  trait JourneyLogger[T <: ReferenceNumber] {
    def name(journey: Journey[T])
  }

  implicit val MrnJourneyLogger = new JourneyLogger[MRN] {

    override def name(journey: Journey[MRN]): String = {

    }
  }
}