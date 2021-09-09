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

import cats.implicits.catsSyntaxApply
import org.scalatest.OptionValues
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.checkYourAnswersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController.selectBasisForClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.whoIsMakingTheClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController.supportingEvidenceKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.{items => declarantTypes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SelectNumberOfClaimsAnswer.{Individual, Scheduled}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SelectNumberOfClaimsAnswer, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.HtmlParseSupport

import scala.collection.JavaConverters._

class CheckYourAnswersSummarySpec
    extends ControllerSpec
    with OptionValues
    with HtmlParseSupport
    with SessionSupport
    with AuthSupport {

  private val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  private val controller: CheckYourAnswersAndSubmitController = instanceOf[CheckYourAnswersAndSubmitController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  "The CYA page" should {

    "display answer summaries for the Single journey" in {
      val (session, claim) = genData(Individual)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = controller.checkAllAnswers(JourneyBindable.Single)(FakeRequest())

      checkPageIsDisplayed(
        result,
        messageFromMessageKey(s"$checkYourAnswersKey.title"),
        doc => {
          val headers = doc.select("#main-content > div > div > h2").content

          val summaries = doc
            .select("#main-content > div > div > dl > div")
            .asScala
            .flatMap { element =>
              val label      = element.select("dt").text()
              val value      = element.select("dd").not(".govuk-summary-list__actions")
              val paragraphs = value.select("p")

              if (paragraphs.isEmpty)
                Seq((label, value.text()))
              else
                paragraphs.content
                  .map(s => (label, s.replace("<br>", " ")))
            }
            .toList

          headers should contain allElementsOf ((
            claim.basisOfClaimAnswer *> Some(s"$checkYourAnswersKey.basis.h2")
          ).toList ++ Seq(
            s"$checkYourAnswersKey.claimant-type.h2",
            s"$checkYourAnswersKey.commodity-details.h2",
            s"$checkYourAnswersKey.attached-documents.h2",
            s"$checkYourAnswersKey.contact-details.h2"
          )).map(messages(_))

          summaries should contain allElementsOf Seq(
            (
              messages(s"$checkYourAnswersKey.claimant-type.l0"),
              messages(
                s"$whoIsMakingTheClaimKey.importer${declarantTypes.indexOf(claim.declarantTypeAnswer.value)}"
              )
            ),
            (
              messages(s"$checkYourAnswersKey.commodities-details.label"),
              claim.commoditiesDetailsAnswer.map(_.value).value
            )
          ) ++ claim.basisOfClaimAnswer.map { answer =>
            (
              messages(s"$checkYourAnswersKey.basis.l0"),
              messages(s"$selectBasisForClaimKey.reason.d${answer.value}")
            )
          }.toList ++ claim.supportingEvidencesAnswer.value.map { uploadDocument =>
            (
              messages(s"$checkYourAnswersKey.attached-documents.label"),
              s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType =>
                messages(s"$supportingEvidenceKey.choose-document-type.document-type.d${documentType.index}")
              )}"
            )
          }.toList
        }
      )
    }

    "display answer summaries for the Scheduled journey" in {
      val (session, claim) = genData(Scheduled)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = controller.checkAllAnswers(JourneyBindable.Scheduled)(FakeRequest())

      checkPageIsDisplayed(
        result,
        messageFromMessageKey(s"$checkYourAnswersKey.title"),
        doc => {
          val headers = doc.select("#main-content > div > div > h2").content

          val summaries = doc
            .select("#main-content > div > div > dl > div")
            .asScala
            .flatMap { element =>
              val label      = element.select("dt").text()
              val value      = element.select("dd").not(".govuk-summary-list__actions")
              val paragraphs = value.select("p")

              if (paragraphs.isEmpty)
                Seq((label, value.text()))
              else
                paragraphs.content
                  .map(s => (label, s.replace("<br>", " ")))
            }
            .toList

          headers should contain allElementsOf ((
            claim.basisOfClaimAnswer *> Some(s"$checkYourAnswersKey.basis.h2")
          ).toList ++ Seq(
            s"$checkYourAnswersKey.claimant-type.h2",
            s"$checkYourAnswersKey.commodity-details.scheduled.h2",
            s"$checkYourAnswersKey.attached-documents.h2",
            s"$checkYourAnswersKey.contact-details.h2"
          )).map(messages(_))

          summaries should contain allElementsOf Seq(
            (
              messages(s"$checkYourAnswersKey.claimant-type.l0"),
              messages(
                s"$whoIsMakingTheClaimKey.importer${declarantTypes.indexOf(claim.declarantTypeAnswer.value)}"
              )
            ),
            (
              messages(s"$checkYourAnswersKey.commodities-details.scheduled.label"),
              claim.commoditiesDetailsAnswer.map(_.value).value
            )
          ) ++ claim.basisOfClaimAnswer.map { answer =>
            (
              messages(s"$checkYourAnswersKey.basis.l0"),
              messages(s"$selectBasisForClaimKey.reason.d${answer.value}")
            )
          }.toList ++ claim.supportingEvidencesAnswer.value.map { uploadDocument =>
            (
              messages(s"$checkYourAnswersKey.attached-documents.label"),
              s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType =>
                messages(s"$supportingEvidenceKey.choose-document-type.document-type.d${documentType.index}")
              )}"
            )
          }.toList
        }
      )
    }
  }

  private def genData(selectNumberOfClaimsAnswer: SelectNumberOfClaimsAnswer): (SessionData, DraftC285Claim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val claim               = sample(genValidDraftClaim(selectNumberOfClaimsAnswer))
    val fillingOutClaim     = FillingOutClaim(ggCredId, signedInUserDetails, claim)
    val session             = SessionData.empty.copy(journeyStatus = Some(fillingOutClaim))
    (session, claim)
  }
}
