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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{SummaryListRow, _}

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourAnswersHelper @Inject() (implicit
  val featureSwitch: FeatureSwitchService
) {

  private val key = "check-your-answers"

  def makeReferenceNumberRowSummary(
    number: Either[EntryNumber, MRN]
  )(implicit messages: Messages): List[SummaryListRow] = {
    val referenceNumber = number match {
      case Left(value)  =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.entry-reference-number.label"))),
          value = Value(Text(value.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.entry-reference-number.label"))
                )
              )
            )
          )
        )
      case Right(value) =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.mrn.label"))),
          value = Value(Text(value.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.mrn.label"))
                )
              )
            )
          )
        )
    }
    List(referenceNumber)
  }

  def makeDeclarationDetailsSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    completeClaim.movementReferenceNumber match {
      case Left(_)  =>
        val rows = List(
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l0"))),
              value = Value(Text(details.dateOfImport.checkYourDetailsDisplayFormat)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l0"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l1"))),
              value = Value(Text(details.placeOfImport)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l1"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l2"))),
              value = Value(Text(details.importerName)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l2"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l3"))),
              value = Value(Text(details.importerEmailAddress.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l3"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l4"))),
              value = Value(Text(details.importerPhoneNumber.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l4"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l5"))),
              value = Value(Text(details.declarantName)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l5"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l6"))),
              value = Value(Text(details.declarantEmailAddress.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l6"))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l7"))),
              value = Value(Text(details.declarantPhoneNumber.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l7"))
                    )
                  )
                )
              )
            )
          }
        )
        rows.flattenOption
      case Right(_) =>
        val rows: List[Option[SummaryListRow]] = List(
          completeClaim.maybeDisplayDeclaration.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l0"))),
              value = Value(Text(details.displayResponseDetail.acceptanceDate)),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l8"))),
              value = Value(Text(formatAmountOfMoneyWithPoundSign(details.totalPaidCharges))),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeName.map { name =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l2"))),
                value = Value(Text(name)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeEmail.map { email =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l3"))),
                value = Value(Text(email)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeTelephone.map { tel =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l4"))),
                value = Value(Text(tel)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeAddress.map { address =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l10"))),
                value = Value(Text(address)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l5"))),
              value = Value(Text(details.declarantName)),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantContactAddress.map { address =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l9"))),
                value = Value(Text(address)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantTelephoneNumber.map { tel =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l7"))),
                value = Value(Text(tel)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantEmailAddress.map { email =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l6"))),
                value = Value(Text(email)),
                actions = None
              )
            }
          }
        )
        rows.flattenOption
    }

  def makeClaimantDetailsSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l0"))),
        value = Value(Text(completeClaim.detailsRegisteredWithCds.fullName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l0"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l1"))),
        value = Value(Text(completeClaim.detailsRegisteredWithCds.emailAddress.value)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l1"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l3"))),
        value = Value(
          Text(
            completeClaim.detailsRegisteredWithCds.contactAddress
              .getAddressLines(messages)
              .mkString(", ")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l3"))
              )
            )
          )
        )
      )
    )

  def makeClaimantDetailsAsImporterSummary(
    completeClaim: CompleteClaim
  )(implicit messages: Messages): List[SummaryListRow] =
    List(
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l0"))),
          value = Value(Text(details.companyName)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterYourContactDetailsController.changeContactDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l0"))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l1"))),
          value = Value(Text(details.emailAddress.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterYourContactDetailsController.changeContactDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l1"))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l2"))),
          value = Value(Text(details.phoneNumber.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterYourContactDetailsController.changeContactDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l2"))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l3"))),
          value = Value(Text(details.contactAddress.getAddressLines(messages).mkString(", "))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterYourContactDetailsController.changeContactDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l3"))
                )
              )
            )
          )
        )
      }
    ).flattenOption

  def makeNorthernIrelandClaimSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    completeClaim.northernIrelandAnswer.fold(List.empty[SummaryListRow])(claimNorthernIrelandAnswer =>
      List(
        SummaryListRow(
          key = Key(Text(messages(s"$key.northern-ireland-claim.label"))),
          value = Value(Text(claimNorthernIrelandAnswer.claimNorthernIrelandAnswer.toString)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.ClaimNorthernIrelandController.changeNorthernIrelandClaim().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.northern-ireland-claim.label"))
                )
              )
            )
          )
        )
      )
    )

  def makeCommodityDetailsSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.commodities-details.label"))),
        value = Value(Text(completeClaim.commodityDetails)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterCommoditiesDetailsController.changeCommoditiesDetails().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.commodities-details.label"))
              )
            )
          )
        )
      )
    )

  def makeBasisAndOrReasonForClaim(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    completeClaim.basisForClaim match {
      case Left(value)  =>
        List(
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l0"))),
            value = Value(Text(value.basisForClaim.string)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectReasonForBasisAndClaimController.changeReasonForClaimAndBasis().url}",
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l0"))
                  )
                )
              )
            )
          ),
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l1"))),
            value = Value(Text(value.reasonForClaim.repr)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectReasonForBasisAndClaimController.changeReasonForClaimAndBasis().url}",
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l1"))
                  )
                )
              )
            )
          )
        )
      case Right(value) =>
        List(
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l0"))),
            value = Value(Text(value.toString)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectBasisForClaimController.changeBasisForClaim().url}",
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l0"))
                  )
                )
              )
            )
          )
        )
    }

  def makeClaimCalculationSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-uk-duty.label"))),
        value = Value(Text(completeClaim.totalUKDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-uk-duty.label"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-eu-duty.label"))),
        value = Value(Text(completeClaim.totalEuDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-eu-duty.label"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.total-claim.label"))),
        value = Value(Text(completeClaim.totalClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.total-claim.label"))
              )
            )
          )
        )
      )
    )

  def makeBankDetailsSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    List(
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-name.label"))),
          value = Value(Text(details.accountName.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-name.label"))
                )
              )
            )
          )
        )
      },
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.sort-code.label"))),
          value = Value(Text(details.sortCode.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.sort-code.label"))
                )
              )
            )
          )
        )
      },
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-number.label"))),
          value = Value(Text(details.accountNumber.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-number.label"))
                )
              )
            )
          )
        )
      }
    ).flattenOption

  def makeSupportingEvidenceSummary(supportingEvidences: List[SupportingEvidence])(implicit
    messages: Messages
  ): List[SummaryListRow] =
    supportingEvidences.zipWithIndex.map { case (document, fileIndex) =>
      SummaryListRow(
        key = Key(Text(messages(s"$key.file-label", fileIndex + 1))),
        value = Value(Text(document.fileName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${fileUploadRoutes.SupportingEvidenceController.checkYourAnswers().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.file-label", fileIndex + 1))
              )
            )
          )
        )
      )
    }
}
