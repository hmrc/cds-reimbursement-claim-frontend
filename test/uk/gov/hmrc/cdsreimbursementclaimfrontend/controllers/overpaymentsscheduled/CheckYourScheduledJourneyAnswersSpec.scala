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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import org.jsoup.nodes
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersSummarySpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DutyTypeSummary

import scala.jdk.CollectionConverters._

class CheckYourScheduledJourneyAnswersSpec extends CheckYourAnswersSummarySpec with SummaryMatchers {

  val controller = instanceOf[CheckYourAnswersAndSubmitController]

  "The C285 Scheduled journey CYA page" should {

    "display answer summaries" in {

      forAll(draftClaimGen(TypeOfClaimAnswer.Scheduled)) { case (session, claim, user) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = controller.checkAllAnswers(FakeRequest())

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("check-your-answers.title"),
          (doc: nodes.Document) => {

            val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
            val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
            val summaryValues = doc.select(".govuk-summary-list__value").eachText()
            val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

            val declaration: Option[DisplayDeclaration]           = claim.displayDeclaration
            val declarationDetails: Option[DisplayResponseDetail] = declaration.map(_.displayResponseDetail)

            val expectedDocuments: String =
              claim.supportingEvidencesAnswer.value
                .map { uploadDocument =>
                  s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType => messages(s"supporting-evidence.choose-document-type.document-type.${UploadDocumentType.keyOf(documentType)}"))}"
                }
                .mkString(" ")

            val scheduledDocument = claim.scheduledDocumentAnswer.map { document =>
              s"${document.uploadDocument.fileName}"
            }

            val claims: Seq[DutyTypeSummary] =
              DutyTypeSummary.buildFrom(claim.claimedReimbursementsAnswer.value)

            val claimSummaries: Seq[(String, Option[String])] = claims.map(claim =>
              (
                messages(s"check-your-answers.claim-calculation.${claim.messageKey}"),
                Some(claim.total.toPoundSterlingString)
              )
            )

            val total: String =
              claims.map(_.total).sum.toPoundSterlingString

            headers.toSeq should containOnlyDefinedElementsOf(
              "First Movement Reference Number (MRN)".expectedAlways,
              "Declaration details".expectedWhen(claim.displayDeclaration),
              "Contact information for this claim".expectedWhen(claim.getClaimantInformation(user.eori)),
              "Basis for claim".expectedAlways,
              "Reason for claim".expectedAlways,
              "Total repayment claim for all MRNs".expectedAlways,
              "Bank details".expectedAlways,
              "Supporting documents".expectedAlways,
              "Were your goods imported into Northern Ireland?".expectedWhen(claim.whetherNorthernIrelandAnswer),
              "Repayment method".expectedWhen(claim.reimbursementMethodAnswer),
              "Scheduled document".expectedAlways,
              "Now send your claim".expectedAlways
            )

            summaries.toSeq should containOnlyDefinedPairsOf(
              Seq(
                "First MRN"                                       -> claim.movementReferenceNumber.map(_.value),
                "Import date"                                     -> declarationDetails.map(_.acceptanceDate),
                "Duties paid"                                     -> declaration.map(_.totalDutiesPaidCharges.toPoundSterlingString),
                "VAT paid"                                        -> declaration.map(_.totalVatPaidCharges.toPoundSterlingString),
                "Importer name"                                   -> declaration.flatMap(_.consigneeName),
                "Importer email"                                  -> declaration.flatMap(_.consigneeEmail),
                "Importer telephone"                              -> declaration.flatMap(_.consigneeTelephone),
                "Importer address"                                -> declaration.flatMap(_.consigneeAddress).map(_.replace("<br />", " ")),
                "Declarant name"                                  -> declaration.map(_.declarantName),
                "Declarant address"                               -> declaration.flatMap(_.declarantContactAddress).map(_.replace("<br />", " ")),
                "This is the basis behind the claim"              -> claim.basisOfClaimAnswer.map(answer =>
                  messages(s"select-basis-for-claim.reason.d${BasisOfOverpaymentClaimsList.indexOf(answer)}")
                ),
                "This is the reason for the claim"                -> claim.additionalDetailsAnswer.map(_.value),
                "Name on the account"                             -> claim.bankAccountDetailsAnswer.map(_.accountName.value),
                "Sort code"                                       -> claim.bankAccountDetailsAnswer.map(_.sortCode.masked),
                "Account number"                                  -> claim.bankAccountDetailsAnswer.map(_.accountNumber.masked),
                "Contact details"                                 -> claim
                  .getClaimantInformation(user.eori)
                  .map(ClaimantInformationSummary.getContactDataString),
                "Contact address"                                 -> claim
                  .getClaimantInformation(user.eori)
                  .map(ClaimantInformationSummary.getAddressDataString),
                "Total"                                           -> Some(total),
                "Uploaded"                                        -> Some(expectedDocuments),
                "Scheduled document"                              -> scheduledDocument,
                "Were your goods imported into Northern Ireland?" -> claim.whetherNorthernIrelandAnswer.map(
                  _.toString()
                )
              )
                ++ claimSummaries
            )
          }
        )
      }

    }
  }
}
