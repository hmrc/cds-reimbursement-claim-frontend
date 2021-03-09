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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{Key, SummaryListRow, Value}

import javax.inject.{Inject, Singleton}

@Singleton
class DeclarationDetailsUtils @Inject() (implicit langs: Langs, messages: MessagesApi) {
  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-declaration-details"

  def declarationSummary(displayDeclaration: DisplayDeclaration): List[SummaryListRow] =
    List(
      makeMrnNumberRow(displayDeclaration.displayResponseDetail.declarationId),
      makeAcceptanceDate(displayDeclaration.displayResponseDetail.acceptanceDate),
      makePaidChargesRow(displayDeclaration.totalPaidCharges),
      makeConsigneeNameRow(displayDeclaration.consigneeName),
      makeConsigneeEmail(displayDeclaration.consigneeEmail),
      makeConsigneeTelephone(displayDeclaration.consigneeTelephone),
      makeConsigneeAddress(displayDeclaration.consigneeAddress),
      makeDeclarantEmailAddress(displayDeclaration.declarantEmailAddress),
      makeDeclarantTelephoneNumber(displayDeclaration.declarantTelephoneNumber),
      makeDeclarantAddress((displayDeclaration.declarantContactAddress))
    ).flattenOption

  private def makeMrnNumberRow(declarationId: String): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.mrn-number.label")(lang))),
        Value(Text(declarationId))
      )
    )

  private def makeAcceptanceDate(date: String): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.import-date.label")(lang))),
        Value(Text(date))
      )
    )

  private def makePaidChargesRow(paid: BigDecimal): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.paid-charges.label")(lang))),
        Value(Text(formatAmountOfMoneyWithPoundSign(paid)))
      )
    )

  private def makeConsigneeNameRow(maybeConsigneeName: Option[String]): Option[SummaryListRow] =
    maybeConsigneeName.map { name =>
      SummaryListRow(
        Key(Text(messages(s"$key.importer-name.label")(lang))),
        Value(Text(name))
      )
    }

  private def makeConsigneeEmail(maybeConsigneeEmail: Option[String]): Option[SummaryListRow] =
    maybeConsigneeEmail.map { email =>
      SummaryListRow(
        Key(Text(messages(s"$key.importer-email.label")(lang))),
        Value(Text(email))
      )
    }

  private def makeConsigneeTelephone(maybeConsigneeTelephone: Option[String]): Option[SummaryListRow] =
    maybeConsigneeTelephone.map { telephone =>
      SummaryListRow(
        Key(Text(messages(s"$key.importer-telephone.label")(lang))),
        Value(Text(telephone))
      )
    }

  private def makeConsigneeAddress(maybeConsigneeAddress: Option[String]): Option[SummaryListRow] =
    maybeConsigneeAddress.map { address =>
      SummaryListRow(
        Key(Text(messages(s"$key.importer-contact-address.label")(lang))),
        Value(Text(address))
      )
    }

  private def makeDeclarantEmailAddress(maybeDeclarantEmailAddress: Option[String]): Option[SummaryListRow] =
    maybeDeclarantEmailAddress.map { email =>
      SummaryListRow(
        Key(Text(messages(s"$key.declarant-email.label")(lang))),
        Value(Text(email))
      )
    }

  private def makeDeclarantTelephoneNumber(maybeDeclarantTelephone: Option[String]): Option[SummaryListRow] =
    maybeDeclarantTelephone.map { telephone =>
      SummaryListRow(
        Key(Text(messages(s"$key.declarant-telephone.label")(lang))),
        Value(Text(telephone))
      )
    }

  private def makeDeclarantAddress(maybeDeclarantAddress: Option[String]): Option[SummaryListRow] =
    maybeDeclarantAddress.map { address =>
      SummaryListRow(
        Key(Text(messages(s"$key.declarant-contact-address.label")(lang))),
        Value(Text(address))
      )
    }

}
