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

import cats.implicits.{catsSyntaxApply, catsSyntaxTuple2Semigroupal}
import org.jsoup.nodes
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.checkYourAnswersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersSummarySpec.DOMDocOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController.selectBasisForClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.whoIsMakingTheClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController.supportingEvidenceKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetails, BigDecimalOps}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer.{BankAccountTransfer, CurrentMonthAdjustment}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{BasisOfClaims, DeclarantTypeAnswers, ReimbursementMethodAnswer, TypeOfClaimAnswer}

class CheckYourSingleJourneyAnswersSpec extends CheckYourAnswersSummarySpec {

  "The CYA page" should {

    "display answer summaries for the Single journey" in {
      val (session, claim) = genData(TypeOfClaimAnswer.Individual)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = controller.checkAllAnswers(JourneyBindable.Single)(FakeRequest())

      checkPageIsDisplayed(
        result,
        messageFromMessageKey(s"$checkYourAnswersKey.title"),
        (doc: nodes.Document) => {
          val headers   = doc.extractHeaders()
          val summaries = doc.extractSummaries()

          headers   should contain allElementsOf (Seq(
            claim.basisOfClaimAnswer *> Some(s"$checkYourAnswersKey.basis.h2"),
            claim.displayDeclaration *> Some(s"$checkYourAnswersKey.declaration-details.h2"),
            claim.mrnContactAddressAnswer *> claim.mrnContactDetailsAnswer *> Some(
              s"$checkYourAnswersKey.contact-details.h2"
            )
          ).flatMap(_.toList) ++ reimbursementMethodHeaders(claim.reimbursementMethodAnswer) ++ Seq(
            s"$checkYourAnswersKey.claimant-type.h2",
            s"$checkYourAnswersKey.claimant-details.h2",
            s"$checkYourAnswersKey.commodity-details.h2",
            s"$checkYourAnswersKey.attached-documents.h2",
            s"$checkYourAnswersKey.reference-number.h2",
            s"$checkYourAnswersKey.claim-calculation.h2"
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
              messages(s"$checkYourAnswersKey.reference-number.label"),
              claim.movementReferenceNumber.value.value
            )
          ) ++ claim.basisOfClaimAnswer.map { answer =>
            (
              messages(s"$checkYourAnswersKey.basis.l0"),
              messages(s"$selectBasisForClaimKey.reason.d${BasisOfClaims.indexOf(answer)}")
            )
          }.toList ++ reimbursementMethodSummaries(
            claim.reimbursementMethodAnswer,
            claim.bankAccountDetailsAnswer
          ) ++ claim.supportingEvidencesAnswer.value.map { uploadDocument =>
            (
              messages(s"$checkYourAnswersKey.attached-documents.label"),
              s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType =>
                messages(s"$supportingEvidenceKey.choose-document-type.document-type.d${documentType.index}")
              )}"
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
            .flatMap(_.toList) ++ (claim.mrnContactDetailsAnswer, claim.mrnContactAddressAnswer)
            .mapN { (details, address) =>
              Seq(
                Some((messages("claimant-details.contact.details"), details.fullName)),
                details.phoneNumber.map { phoneNumber =>
                  (messages("claimant-details.contact.details"), phoneNumber.value)
                },
                Some((messages("claimant-details.contact.details"), details.emailAddress.value)),
                Some(
                  (
                    messages("claimant-details.contact.address"),
                    address.line1
                  )
                ),
                address.line2.map { line2 =>
                  (
                    messages("claimant-details.contact.address"),
                    line2
                  )
                },
                address.line3.map { line3 =>
                  (
                    messages("claimant-details.contact.address"),
                    line3
                  )
                },
                Some(
                  (
                    messages("claimant-details.contact.address"),
                    address.line4
                  )
                ),
                Some(
                  (
                    messages("claimant-details.contact.address"),
                    address.postcode
                  )
                ),
                Some(
                  (
                    messages("claimant-details.contact.address"),
                    messages(address.country.messageKey)
                  )
                )
              ).flatMap(_.toList)
            }
            .getOrElse(Seq.empty)
        }
      )
    }
  }

  private def reimbursementMethodHeaders(
    reimbursementMethodAnswer: Option[ReimbursementMethodAnswer]
  ): Seq[String] =
    reimbursementMethodAnswer match {
      case Some(CurrentMonthAdjustment) => Seq(s"$checkYourAnswersKey.reimbursement-method.h2")
      case Some(BankAccountTransfer)    =>
        Seq(s"$checkYourAnswersKey.reimbursement-method.h2", s"$checkYourAnswersKey.bank-details.h2")
      case _                            => Seq(s"$checkYourAnswersKey.bank-details.h2")
    }

  private def reimbursementMethodSummaries(
    reimbursementMethodAnswer: Option[ReimbursementMethodAnswer],
    bankDetails: Option[BankAccountDetails]
  ): Seq[(String, String)] =
    (reimbursementMethodAnswer, bankDetails) match {
      case (Some(CurrentMonthAdjustment), _)                     =>
        Seq(
          (
            messages(s"$checkYourAnswersKey.reimbursement-method.label"),
            messages(s"$checkYourAnswersKey.reimbursement-method.cma")
          )
        )
      case (Some(BankAccountTransfer), Some(bankAccountDetails)) =>
        Seq(
          (
            messages(s"$checkYourAnswersKey.reimbursement-method.label"),
            messages(s"$checkYourAnswersKey.reimbursement-method.bt")
          )
        ) ++ bankAccountDetailsSummaries(bankAccountDetails)
      case (None, Some(bankAccountDetails))                      =>
        bankAccountDetailsSummaries(bankAccountDetails)
    }

  private def bankAccountDetailsSummaries(bankAccountDetails: BankAccountDetails): Seq[(String, String)] = Seq(
    (
      messages(s"$checkYourAnswersKey.bank-details.account-name.label"),
      bankAccountDetails.accountName.value
    ),
    (
      messages(s"$checkYourAnswersKey.bank-details.sort-code.label"),
      bankAccountDetails.sortCode.value
    ),
    (
      messages(s"$checkYourAnswersKey.bank-details.account-number.label"),
      bankAccountDetails.accountNumber.value
    )
  )
}
