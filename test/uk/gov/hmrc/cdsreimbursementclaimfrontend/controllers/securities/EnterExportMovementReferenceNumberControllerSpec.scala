/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import cats.data.EitherT
import org.jsoup.nodes.Document
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithClaimGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.EnterExportMovementReferenceNumberHelper
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class EnterExportMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithClaimGenerator[SecuritiesClaim]
    with Logging {

  val enterExportMovementReferenceNumberSingleKey: String          = "enter-export-movement-reference-number"
  val enterExportMovementReferenceNumberSingleKeyAndSubKey: String =
    s"$enterExportMovementReferenceNumberSingleKey.securities"

  val enterExportMovementReferenceNumberMultipleKey: String          = "enter-export-movement-reference-number.next"
  val enterExportMovementReferenceNumberMultipleKeyAndSubKey: String =
    s"$enterExportMovementReferenceNumberMultipleKey.securities"

  val enterExportMovementReferenceNumberYesNoKey: String =
    s"$enterExportMovementReferenceNumberSingleKey.securities.yes-no"

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[ClaimService].toInstance(mockClaimsService),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterExportMovementReferenceNumberController =
    instanceOf[EnterExportMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def mockGetImportDeclaration(expectedMrn: MRN, response: Either[Error, Option[ImportDeclaration]]) =
    (mockClaimsService
      .getImportDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  def validateChooseExportMethodFirstPage(doc: Document): Assertion = {
    val headerHtml     = doc.select(".govuk-label--xl").html()
    val input          = doc.select(s"#$enterExportMovementReferenceNumberSingleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList
    val h2             = doc.select(".govuk-fieldset__legend--m").eachText().asScala
    val radioLabels    = doc.select(".govuk-radios__item label").eachText().asScala
    val radioItems     = doc.select(".govuk-radios__item input").eachAttr("value").asScala

    headerHtml          should ===(messages(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"))
    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
    h2                  should ===(List(messageFromMessageKey(enterExportMovementReferenceNumberYesNoKey)))
    radioLabels.length  should ===(2)
    radioItems.length   should ===(2)
  }

  def validateChooseExportMethodNextPage(doc: Document): Assertion = {
    val input          = doc.select(s"#$enterExportMovementReferenceNumberMultipleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
  }

  def validateChooseExportMethodNextPageWithYesNoOptions(doc: Document): Assertion = {
    val input          = doc.select(s"#$enterExportMovementReferenceNumberMultipleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList
    val h2             = doc.select(".govuk-fieldset__legend--m").eachText().asScala
    val radioLabels    = doc.select(".govuk-radios__item label").eachText().asScala
    val radioItems     = doc.select(".govuk-radios__item input").eachAttr("value").asScala

    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
    h2                  should ===(List(messageFromMessageKey(enterExportMovementReferenceNumberYesNoKey)))
    radioLabels.length  should ===(2)
    radioItems.length   should ===(2)
  }

  "Movement Reference Number Controller" when {

    "Enter export MRN page" must {

      def performAction(mrnIndex: Int): Future[Result] =
        if mrnIndex === 0 then controller.showFirst(FakeRequest())
        else controller.showNext(mrnIndex + 1)(FakeRequest())

      "show first enter export MRN page" must {

        "display the page if acc14 is present" in forAllWith(
          ClaimGenerator(
            mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen,
            buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(0),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
            validateChooseExportMethodFirstPage
          )
        }

        "display the page with export MRN populated" in forAllWith(
          ClaimGenerator(
            testParamsGenerator = mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
            )
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(0),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
            doc =>
              doc.select(s"#$enterExportMovementReferenceNumberSingleKey").attr("value") shouldBe "19GB03I52858027001"
          )
        }

        "redirect to next page when method of disposal is not exported in single or multiple shipments" in {
          val gen   = mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen.sample.getOrElse(
            fail("Failed to generate claim data")
          )
          val claim = buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal(gen)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(performAction(0), routes.ConfirmFullRepaymentController.showFirst)
        }

        "redirect to next page when method of disposal is not exported in single or multiple shipments for single security deposit" in {
          val gen   =
            SecuritiesSingleClaimGenerators.mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen.sample.getOrElse(
              fail("Failed to generate claim data")
            )
          val claim =
            SecuritiesSingleClaimGenerators.buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal(gen)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(performAction(0), routes.ChoosePayeeTypeController.show)
        }

        "redirect to next page when reason for security is not temporary admission" in {
          val gen          = mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen.sample.getOrElse(
            fail("Failed to generate claim data")
          )
          val claim        = buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal(gen)
          val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(
            claim,
            answers => answers.copy(reasonForSecurity = Some(MissingPreferenceCertificate))
          )

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkIsRedirect(performAction(0), routes.ConfirmFullRepaymentController.showFirst)
        }

        "redirect to choose export method page when method of disposal is not found" in {
          val gen          = mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen.sample.getOrElse(
            fail("Failed to generate claim data")
          )
          val claim        = buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal(gen)
          val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(
            claim,
            answers => answers.copy(temporaryAdmissionMethodsOfDisposal = None)
          )

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkIsRedirect(performAction(0), routes.ChooseExportMethodController.show)
        }
      }

      "show enter next export MRN page" must {

        "display the page" in forAllWith(
          ClaimGenerator(
            testParamsGenerator = mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(mrnIndex),
            EnterExportMovementReferenceNumberHelper.title(mrnIndex + 1),
            validateChooseExportMethodNextPage
          )
        }
      }

      "display the page with yes no options on when user has seen check your answers page and page index is equal to export MRNs size" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithTaRfsWithImportDeclarationGen,
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"))
          )
        )
      ) { case (claim, _) =>
        val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(
          claim,
          answers => answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = true))
        )
        val mrnIndex     = 1

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkPageIsDisplayed(
          performAction(mrnIndex),
          EnterExportMovementReferenceNumberHelper.title(mrnIndex + 1),
          validateChooseExportMethodNextPageWithYesNoOptions
        )
      }

      "redirect to show first when the page index is out of bounds and there are no export MRNs set" in {
        val gen                 = mrnWithTaRfsWithImportDeclarationGen.sample.getOrElse(fail("Failed to generate claim data"))
        val claim               = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(gen)
        val outOfBoundsMrnIndex = 1

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(outOfBoundsMrnIndex),
          routes.EnterExportMovementReferenceNumberController.showFirst
        )
      }

      "redirect to show next when the page index is out of bounds and there are other export MRNs set" in {
        val gen                 = mrnWithTaRfsWithImportDeclarationGen.sample.getOrElse(fail("Failed to generate claim data"))
        val claim               = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
          Seq(MRN("19GB03I52858027001"))
        )(gen)
        val outOfBoundsMrnIndex = 2

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(outOfBoundsMrnIndex),
          routes.EnterExportMovementReferenceNumberController.showNext(2)
        )
      }
    }

    "Submit export MRN page" must {

      def performAction(mrnIndex: Int, data: Seq[(String, String)]): Future[Result] =
        if mrnIndex === 0 then controller.submitFirst()(FakeRequest().withFormUrlEncodedBody(data*))
        else controller.submitNext(mrnIndex + 1)(FakeRequest().withFormUrlEncodedBody(data*))

      "submit first export MRN" must {

        "save an export MRN if valid and continue to next page when no selected" in forAllWith(
          ClaimGenerator(
            mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen,
            buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exampleMrn, Right(None))
            mockStoreSession(
              SessionData(
                claim.submitExportMovementReferenceNumber(0, exampleMrn).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              0,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString,
                enterExportMovementReferenceNumberYesNoKey  -> "false"
              )
            ),
            routes.ConfirmFullRepaymentController.showFirst
          )
        }

        "save an export MRN if valid and continue to check export MRNs page when no selected and more than 1 export MRNs entered" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"))
            )
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exampleMrn, Right(None))
            mockStoreSession(
              SessionData(
                claim.submitExportMovementReferenceNumber(0, exampleMrn).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              0,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString,
                enterExportMovementReferenceNumberYesNoKey  -> "false"
              )
            ),
            routes.CheckExportMovementReferenceNumbersController.show
          )
        }

        "save an export MRN if valid and continue to enter next export MRN page when yes selected" in forAllWith(
          ClaimGenerator(
            mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen,
            buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exampleMrn, Right(None))
            mockStoreSession(
              SessionData(
                claim.submitExportMovementReferenceNumber(0, exampleMrn).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              0,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString,
                enterExportMovementReferenceNumberYesNoKey  -> "true"
              )
            ),
            routes.EnterExportMovementReferenceNumberController.showNext(2)
          )
        }

        "reject an export MRN if declaration exists for it" in forAllWith(
          ClaimGenerator(
            mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen,
            buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal
          )
        ) { case (claim, _) =>
          val exportMrnAndDecl = mrnWithImportDeclarationGen.sample.get

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exportMrnAndDecl._1, Right(Some(exportMrnAndDecl._2)))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              0,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> exportMrnAndDecl._1.value,
                enterExportMovementReferenceNumberYesNoKey  -> "false"
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKey.securities.error.import")
          )
        }

        "reject an empty export MRN" in forAllWith(
          ClaimGenerator(
            mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen,
            buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal
          )
        ) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              0,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> "",
                enterExportMovementReferenceNumberYesNoKey  -> "false"
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKey.error.required")
          )
        }

        "reject a duplicate export MRN" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 0

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(MRN("19GB03I52858027002"), Right(None))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberSingleKey -> "19GB03I52858027002",
                enterExportMovementReferenceNumberYesNoKey  -> "true"
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKey.securities.error.duplicate-number")
          )
        }
      }

      "submit next export MRN" must {

        "save an export MRN if valid and continue to check export MRNs page" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exampleMrn, Right(None))
            mockStoreSession(
              SessionData(
                claim.submitExportMovementReferenceNumber(mrnIndex, exampleMrn).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString
              )
            ),
            routes.CheckExportMovementReferenceNumbersController.show
          )
        }

        "save an export MRN if valid and continue to enter next export MRN when user has seen CYA page and selects yes" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(
            claim,
            answers => answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = true))
          )

          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
            mockGetImportDeclaration(exampleMrn, Right(None))
            mockStoreSession(
              SessionData(
                updatedClaim.submitExportMovementReferenceNumber(mrnIndex, exampleMrn).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString,
                enterExportMovementReferenceNumberYesNoKey    -> "true"
              )
            ),
            routes.EnterExportMovementReferenceNumberController.showNext(mrnIndex + 2)
          )
        }

        "save the same export MRN if valid and continue to check your answers page when user has seen CYA page and selects no" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"))
            )
          )
        ) { case (claim, _) =>
          val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(
            claim,
            answers => answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = true))
          )

          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
            mockGetImportDeclaration(MRN("19GB03I52858027002"), Right(None))
            mockStoreSession(
              SessionData(
                updatedClaim.submitExportMovementReferenceNumber(mrnIndex, MRN("19GB03I52858027002")).getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> "19GB03I52858027002",
                enterExportMovementReferenceNumberYesNoKey    -> "false"
              )
            ),
            routes.CheckYourAnswersController.show
          )
        }

        "reject an export MRN if declaration exists for it" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 1

          val exportMrnAndDecl = mrnWithImportDeclarationGen.sample.get

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(exportMrnAndDecl._1, Right(Some(exportMrnAndDecl._2)))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> exportMrnAndDecl._1.value
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title", "second"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKey.securities.error.import")
          )
        }

        "reject an empty export MRN" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> ""
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title", "second"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKey.error.required")
          )
        }

        "reject a duplicate export MRN" in forAllWith(
          ClaimGenerator(
            mrnWithTaRfsWithImportDeclarationGen,
            claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
              Seq(MRN("19GB03I52858027001"))
            )
          )
        ) { case (claim, _) =>
          val mrnIndex = 1

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(MRN("19GB03I52858027001"), Right(None))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> "19GB03I52858027001"
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title", "second"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.error.duplicate-number")
          )
        }

        "reject an export MRN at out of bounds index" in {
          val claimGen =
            mrnWithTaRfsWithImportDeclarationGen.sample.getOrElse(fail("Failed to generate claim data"))
          val claim    = buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"))
          )(claimGen)

          val mrnIndex = 2

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(MRN("19GB03I52858027002"), Right(None))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              mrnIndex,
              Seq(
                enterExportMovementReferenceNumberMultipleKey -> "19GB03I52858027002"
              )
            ),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title", "third"),
            messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.error.import")
          )
        }
      }
    }
  }
}
