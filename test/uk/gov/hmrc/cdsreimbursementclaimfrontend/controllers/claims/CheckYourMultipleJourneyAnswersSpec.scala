/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.checkYourAnswersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersSummarySpec.DOMDocOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController.selectBasisForClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.whoIsMakingTheClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController.supportingEvidenceKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AnswersOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

class CheckYourMultipleJourneyAnswersSpec extends CheckYourAnswersSummarySpec with CheckCDSDetails {

  "The CYA page" should {

    "display answer summaries for the Multiple journey" in {
      val (session, claim)                              = genData(TypeOfClaimAnswer.Multiple)
      val maybeFillingOutClaim: Option[FillingOutClaim] = session.journeyStatus.collect {
        case fillingOutClaim: FillingOutClaim => fillingOutClaim
      }

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = controller.checkAllAnswers(JourneyBindable.Multiple)(FakeRequest())

      checkPageIsDisplayed(
        result,
        messageFromMessageKey(s"$checkYourAnswersKey.title"),
        doc => {
          val headers   = doc.extractHeaders()
          val summaries = doc.extractSummaries()

          headers   should contain allElementsOf (Seq(
            claim.basisOfClaimAnswer *> Some(s"$checkYourAnswersKey.basis.h2"),
            claim.displayDeclaration *> Some(s"$checkYourAnswersKey.declaration-details.h2"),
            claim.extractEstablishmentAddress *> Some(s"$checkYourAnswersKey.claimant-details.h2")
          ).flatMap(_.toList) ++ Seq(
            s"$checkYourAnswersKey.claimant-type.h2",
            s"$checkYourAnswersKey.commodity-details.h2",
            s"$checkYourAnswersKey.attached-documents.h2",
            s"$checkYourAnswersKey.reference-number.multiple.h2"
          )).map(messages(_))

          summaries should contain allElementsOf Seq(
            (
              messages(s"$checkYourAnswersKey.claimant-type.l0"),
              messages(
                s"$whoIsMakingTheClaimKey.importer${DeclarantTypeAnswers.indexOf(claim.declarantTypeAnswer.value)}"
              )
            ),
            (
              messages(s"$checkYourAnswersKey.commodities-details.label"),
              claim.commoditiesDetailsAnswer.map(_.value).value
            ),
            (
              messages(s"$checkYourAnswersKey.reference-number.multiple.label"),
              claim.movementReferenceNumber.value.value
            )
          ) ++ claim.associatedMRNsAnswer.list.zipWithIndex.map { case (mrn, index) =>
            (
              messages(
                s"$checkYourAnswersKey.reference-number.associated-mrn-label",
                AssociatedMrnIndex.fromListIndex(index).ordinalNumeral.capitalize
              ),
              mrn.value
            )
          } ++ claim.basisOfClaimAnswer.map { answer =>
            (
              messages(s"$checkYourAnswersKey.basis.l0"),
              messages(s"$selectBasisForClaimKey.reason.d${BasisOfClaims.indexOf(answer)}")
            )
          }.toList ++ claim.supportingEvidencesAnswer.value.map { uploadDocument =>
            (
              messages(s"$checkYourAnswersKey.attached-documents.label"),
              s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType => messages(s"$supportingEvidenceKey.choose-document-type.document-type.${UploadDocumentType.keyOf(documentType)}"))}"
            )
          }.toList ++ claim.displayDeclaration.toList
            .flatMap { declaration =>
              Seq(
                Some(
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.import-date-label"),
                    declaration.displayResponseDetail.acceptanceDate
                  )
                ),
                Some(
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.paid-charges-label"),
                    declaration.totalPaidCharges.toPoundSterlingString
                  )
                ),
                declaration.consigneeName.map { name =>
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.importer-name-label"),
                    name
                  )
                },
                declaration.consigneeEmail.map { email =>
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.importer-email-label"),
                    email
                  )
                },
                declaration.consigneeAddress.map { address =>
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.importer-address-label"),
                    address.replace("<br />", " ")
                  )
                },
                Some(
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.declarant-name-label"),
                    declaration.declarantName
                  )
                ),
                declaration.declarantContactAddress.map { address =>
                  (
                    messages(s"$checkYourAnswersKey.declaration-details.declarant-address-label"),
                    address.replace("<br />", " ")
                  )
                }
              )
            }
            .flatMap(_.toList) ++ contactDetailsFromCDS(
            claim,
            maybeFillingOutClaim.map(_.signedInUserDetails.verifiedEmail)
          )
        }
      )
    }
  }
}
