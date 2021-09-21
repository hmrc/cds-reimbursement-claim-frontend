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

import cats.syntax.all._
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourAnswersHelper @Inject() (implicit val featureSwitch: FeatureSwitchService) {

  private val key = "check-your-answers"

  def makeDeclarationDetailsSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    completeClaim.movementReferenceNumber.value match {
      case Left(_)  => List()
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
        value = Value(Text(completeClaim.detailsRegisteredWithCdsAnswer.fullName)),
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
        value = Value(Text(completeClaim.detailsRegisteredWithCdsAnswer.emailAddress.value)),
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
            completeClaim.detailsRegisteredWithCdsAnswer.contactAddress
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

  def makeNorthernIrelandClaimSummary(
    completeClaim: CompleteClaim
  )(implicit messages: Messages, journey: JourneyBindable): List[SummaryListRow] =
    completeClaim.northernIrelandAnswer.fold(List.empty[SummaryListRow])(claimNorthernIrelandAnswer =>
      List(
        SummaryListRow(
          key = Key(Text(messages(s"$key.northern-ireland-claim.label"))),
          value = Value(Text(claimNorthernIrelandAnswer.toString)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.ClaimNorthernIrelandController.changeNorthernIrelandClaim(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.northern-ireland-claim.label"))
                )
              )
            )
          )
        )
      )
    )

  def makeClaimCalculationSummary(completeClaim: CompleteClaim)(implicit messages: Messages): List[SummaryListRow] =
    List(
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-uk-duty.label"))),
        value = Value(Text(completeClaim.totalUKDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaimSummary().url}",
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
                href = s"${routes.EnterClaimController.checkClaimSummary().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-eu-duty.label"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(Text(messages(s"$key.claim-excise-duty.label"))),
        value = Value(Text(completeClaim.totalExciseDutyClaim)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.checkClaimSummary().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-excise-duty.label"))
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
                href = s"${routes.EnterClaimController.checkClaimSummary().url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.total-claim.label"))
              )
            )
          )
        )
      )
    )

  def makeBankDetailsSummary(
    completeClaim: CompleteClaim
  )(implicit messages: Messages, journey: JourneyBindable): List[SummaryListRow] =
    List(
      completeClaim.bankDetails.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-name.label"))),
          value = Value(Text(details.accountName.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
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
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
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
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-number.label"))
                )
              )
            )
          )
        )
      }
    ).flattenOption
}
