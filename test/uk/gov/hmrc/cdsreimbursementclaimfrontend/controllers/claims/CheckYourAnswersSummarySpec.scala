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

import org.jsoup.nodes
import org.scalatest.OptionValues
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.HtmlParseSupport

abstract class CheckYourAnswersSummarySpec
    extends ControllerSpec
    with OptionValues
    with SessionSupport
    with AuthSupport {

  private val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  protected val controller: CheckYourAnswersAndSubmitController =
    instanceOf[CheckYourAnswersAndSubmitController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  protected def genData(maybeTypeOfClaim: TypeOfClaim): (SessionData, DraftClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val claim               = sample(genValidDraftClaim(maybeTypeOfClaim))
    val fillingOutClaim     = FillingOutClaim(ggCredId, signedInUserDetails, claim)
    val session             = SessionData.empty.copy(journeyStatus = Some(fillingOutClaim))
    (session, claim)
  }
}

object CheckYourAnswersSummarySpec extends HtmlParseSupport {
  import scala.collection.JavaConverters._

  implicit class DOMDocOps(val document: nodes.Document) extends AnyVal {

    def extractHeaders(): Seq[String] =
      document
        .select("#main-content > div > div > h2")
        .content

    def extractSummaries(): Seq[(String, String)] =
      document
        .select("#main-content > div > div > dl > div")
        .asScala
        .flatMap { element =>
          val label      = element.select("dt").text()
          val value      = element.select("dd").not(".govuk-summary-list__actions")
          val paragraphs = value.select("p")

          if (paragraphs.isEmpty)
            Seq((label, value.text()))
          else
            paragraphs.content.map { s =>
              (label, s.replace("<br>", " "))
            }
        }
        .toList
  }
}
