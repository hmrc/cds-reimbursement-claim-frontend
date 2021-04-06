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
import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{SummaryListRow, _}

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourAnswersHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-your-answers"

  def makeReferenceNumberRowSummary(number: Either[EntryNumber, MRN]): List[SummaryListRow] = {
    val referenceNumber = number match {
      case Left(value)  =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.entry-reference-number.label")(lang))),
          value = Value(Text(value.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.entry-reference-number.label")(lang))
                )
              )
            )
          )
        )
      case Right(value) =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.mrn.label")(lang))),
          value = Value(Text(value.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.mrn.label")(lang))
                )
              )
            )
          )
        )
    }
    List(referenceNumber)
  }

  def makeDeclarationDetailsSummary(completeClaim: CompleteClaim): List[SummaryListRow] =
    completeClaim.movementReferenceNumber match {
      case Left(_)  =>
        val rows = List(
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l0")(lang))),
              value = Value(Text(details.dateOfImport.checkYourDetailsDisplayFormat)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l0")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l1")(lang))),
              value = Value(Text(details.placeOfImport)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l1")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l2")(lang))),
              value = Value(Text(details.importerName)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l2")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l3")(lang))),
              value = Value(Text(details.importerEmailAddress.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l3")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l4")(lang))),
              value = Value(Text(details.importerPhoneNumber.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l4")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l5")(lang))),
              value = Value(Text(details.declarantName)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l5")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l6")(lang))),
              value = Value(Text(details.declarantEmailAddress.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l6")(lang))
                    )
                  )
                )
              )
            )
          },
          completeClaim.entryDeclarationDetails.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l7")(lang))),
              value = Value(Text(details.declarantPhoneNumber.value)),
              actions = Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${routes.EnterDeclarationDetailsController.changeDeclarationDetails().url}",
                      content = Text(messages("cya.change")(lang)),
                      visuallyHiddenText = Some(messages(s"$key.declaration-details.l7")(lang))
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
              key = Key(Text(messages(s"$key.declaration-details.l0")(lang))),
              value = Value(Text(details.displayResponseDetail.acceptanceDate)),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l8")(lang))),
              value = Value(Text(formatAmountOfMoneyWithPoundSign(details.totalPaidCharges))),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeName.map { name =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l2")(lang))),
                value = Value(Text(name)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeEmail.map { email =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l3")(lang))),
                value = Value(Text(email)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeTelephone.map { tel =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l4")(lang))),
                value = Value(Text(tel)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.consigneeAddress.map { address =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l10")(lang))),
                value = Value(Text(address)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.map { details =>
            SummaryListRow(
              key = Key(Text(messages(s"$key.declaration-details.l5")(lang))),
              value = Value(Text(details.declarantName)),
              actions = None
            )
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantContactAddress.map { address =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l9")(lang))),
                value = Value(Text(address)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantTelephoneNumber.map { tel =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l7")(lang))),
                value = Value(Text(tel)),
                actions = None
              )
            }
          },
          completeClaim.maybeDisplayDeclaration.flatMap { details =>
            details.declarantEmailAddress.map { email =>
              SummaryListRow(
                key = Key(Text(messages(s"$key.declaration-details.l6")(lang))),
                value = Value(Text(email)),
                actions = None
              )
            }
          }
        )
        rows.flattenOption
    }

  def makeClaimantDetailsSummary(
    completeClaim: CompleteClaim,
    request: RequestWithSessionData[_]
  ): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l0")(lang))),
        value = Value(Text(completeClaim.claimantDetailsAsIndividual.fullName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeClaimantDetailsAsIndividual().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l0")(lang))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l1")(lang))),
        value = Value(Text(completeClaim.claimantDetailsAsIndividual.emailAddress.value)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeClaimantDetailsAsIndividual().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l1")(lang))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claimant-details.l3")(lang))),
        value = Value(
          Text(
            completeClaim.claimantDetailsAsIndividual.contactAddress
              .getAddressLines(messages.preferred(request))
              .mkString(", ")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterDetailsRegisteredWithCdsController.changeClaimantDetailsAsIndividual().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.claimant-details.l3")(lang))
              )
            )
          )
        )
      )
    )

  def makeClaimantDetailsAsImporterSummary(
    completeClaim: CompleteClaim,
    request: RequestWithSessionData[_]
  ): List[SummaryListRow] =
    List(
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l0")(lang))),
          value = Value(Text(details.companyName)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href =
                    s"${routes.EnterClaimantDetailsAsImporterCompanyController.changeClaimantDetailsAsImporterCompany().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l0")(lang))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l1")(lang))),
          value = Value(Text(details.emailAddress.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href =
                    s"${routes.EnterClaimantDetailsAsImporterCompanyController.changeClaimantDetailsAsImporterCompany().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l1")(lang))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l2")(lang))),
          value = Value(Text(details.phoneNumber.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href =
                    s"${routes.EnterClaimantDetailsAsImporterCompanyController.changeClaimantDetailsAsImporterCompany().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l2")(lang))
                )
              )
            )
          )
        )
      },
      completeClaim.claimantDetailsAsImporterCompany.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l3")(lang))),
          value = Value(Text(details.contactAddress.getAddressLines(messages.preferred(request)).mkString(", "))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href =
                    s"${routes.EnterClaimantDetailsAsImporterCompanyController.changeClaimantDetailsAsImporterCompany().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l3")(lang))
                )
              )
            )
          )
        )
      }
    ).flattenOption

  def makeCommodityDetailsSummary(completeClaim: CompleteClaim): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.commodities-details.label")(lang))),
        value = Value(Text(completeClaim.commodityDetails)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterCommoditiesDetailsController.changeCommoditiesDetails().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.commodities-details.label")(lang))
              )
            )
          )
        )
      )
    )

  def makeBasisAndOrReasonForClaim(completeClaim: CompleteClaim): List[SummaryListRow] =
    completeClaim.basisForClaim match {
      case Left(value)  =>
        List(
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l0")(lang))),
            value = Value(Text(value.basisForClaim)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectReasonForBasisAndClaimController.changeReasonForClaimAndBasis().url}",
                    content = Text(messages("cya.change")(lang)),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l0")(lang))
                  )
                )
              )
            )
          ),
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l1")(lang))),
            value = Value(Text(value.reasonForClaim.repr)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectReasonForBasisAndClaimController.changeReasonForClaimAndBasis().url}",
                    content = Text(messages("cya.change")(lang)),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l1")(lang))
                  )
                )
              )
            )
          )
        )
      case Right(value) =>
        List(
          SummaryListRow(
            key = Key(Text(messages(s"$key.reason-and-basis.l0")(lang))),
            value = Value(Text(value.toString)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.SelectBasisForClaimController.changeBasisForClaim().url}",
                    content = Text(messages("cya.change")(lang)),
                    visuallyHiddenText = Some(messages(s"$key.reason-and-basis.l0")(lang))
                  )
                )
              )
            )
          )
        )
    }

  def makeClaimCalculationSummary(completeClaim: CompleteClaim): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-uk-duty.label")(lang))),
        value = Value(Text(completeClaim.totalUKDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.claim-uk-duty.label")(lang))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-eu-duty.label")(lang))),
        value = Value(Text(completeClaim.totalEuDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.claim-eu-duty.label")(lang))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.total-claim.label")(lang))),
        value = Value(Text(completeClaim.totalClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaim().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.total-claim.label")(lang))
              )
            )
          )
        )
      )
    )

  def makeBankDetailsSummary(completeClaim: CompleteClaim): List[SummaryListRow] =
    List(
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-name.label")(lang))),
          value = Value(Text(details.accountName.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-name.label")(lang))
                )
              )
            )
          )
        )
      },
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.sort-code.label")(lang))),
          value = Value(Text(details.sortCode.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.sort-code.label")(lang))
                )
              )
            )
          )
        )
      },
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-number.label")(lang))),
          value = Value(Text(details.accountNumber.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails().url}",
                  content = Text(messages("cya.change")(lang)),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-number.label")(lang))
                )
              )
            )
          )
        )
      }
    ).flattenOption

  def makeSupportingEvidenceSummary(supportingEvidences: List[SupportingEvidence]): List[SummaryListRow] =
    supportingEvidences.zipWithIndex.map { case (document, fileIndex) =>
      SummaryListRow(
        key = Key(Text(messages(s"$key.file-label", fileIndex + 1)(lang))),
        value = Value(Text(document.fileName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${fileUploadRoutes.SupportingEvidenceController.checkYourAnswers().url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"$key.file-label", fileIndex + 1)(lang))
              )
            )
          )
        )
      )
    }
}
