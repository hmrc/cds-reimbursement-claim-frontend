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

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.checkYourAnswersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email

trait CheckCDSDetails {
  val messages: Messages

  def contactDetailsFromCDS(claim: DraftClaim, maybeVerifiedEmail: Option[Email]): Seq[(String, String)] = {
    val establishmentAddress = claim.extractEstablishmentAddress

    val requiredContactDetailsFromCDS: Option[Seq[(String, String)]] = maybeVerifiedEmail
      .map { verifiedEmail =>
        val cdsContactDetails = claim.extractDetailsRegisteredWithCDS(verifiedEmail)
        Seq(
          cdsContactDetails.name.map { name =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.cds.contact.details"),
              name
            )
          },
          cdsContactDetails.phoneNumber.map { phoneNumber =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.cds.contact.details"),
              phoneNumber.value
            )
          },
          cdsContactDetails.email.map { email =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.cds.contact.details"),
              email.value
            )
          }
        ).flatten(Option.option2Iterable)
      }

    val requiredAddressFromCDS: Option[Seq[(String, String)]] = establishmentAddress.map { address =>
      Seq(
        Some(
          (
            messages(s"$checkYourAnswersKey.claimant-details.cds.contact.address"),
            address.addressLine1
          )
        ),
        address.addressLine2.map { line =>
          (
            messages(s"$checkYourAnswersKey.claimant-details.cds.contact.address"),
            line
          )
        },
        address.addressLine3.map { line =>
          (
            messages(s"$checkYourAnswersKey.claimant-details.cds.contact.address"),
            line
          )
        },
        address.postalCode.map { postCode =>
          (
            messages(s"$checkYourAnswersKey.claimant-details.cds.contact.address"),
            postCode
          )
        },
        Some(
          (
            messages(s"$checkYourAnswersKey.claimant-details.cds.contact.address"),
            messages(s"country.${address.countryCode}")
          )
        )
      ).flatten(Option.option2Iterable)
    }

    val includeAdditionalContacts = claim.mrnContactDetailsAnswer.isDefined && claim.mrnContactAddressAnswer.isDefined

    val requiredAdditionalContact = {
      val additionalContactKey =
        if (includeAdditionalContacts) {
          "yes"
        } else {
          "no"
        }
      Seq(
        (
          messages(s"$checkYourAnswersKey.claimant-details.contact.additional"),
          messages(s"generic.$additionalContactKey")
        )
      )
    }

    val requiredAdditionalContactDetails: Option[Seq[(String, String)]] = if (includeAdditionalContacts) {
      claim.mrnContactDetailsAnswer.map { contact =>
        Seq(
          Some(
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.details"),
              contact.fullName
            )
          ),
          contact.phoneNumber.map { phoneNumber =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.details"),
              phoneNumber.value
            )
          },
          Some(
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.details"),
              contact.emailAddress.value
            )
          )
        ).flatten(Option.option2Iterable)
      }
    } else {
      None
    }

    val requiredAdditionalAddress: Option[Seq[(String, String)]] = if (includeAdditionalContacts) {
      claim.mrnContactAddressAnswer.map { address =>
        Seq(
          Some(
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.address"),
              address.line1
            )
          ),
          address.line2.map { line =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.address"),
              line
            )
          },
          address.line3.map { line =>
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.address"),
              line
            )
          },
          Some(
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.address"),
              address.postcode
            )
          ),
          Some(
            (
              messages(s"$checkYourAnswersKey.claimant-details.additional.contact.address"),
              messages(s"country.${address.country.code}")
            )
          )
        ).flatten(Option.option2Iterable)
      }
    } else {
      None
    }

    val fullListOfRequirements = for {
      cdsContact        <- requiredContactDetailsFromCDS
      if establishmentAddress.isDefined
      cdsAddress        <- requiredAddressFromCDS
      additionalContact <- requiredAdditionalContactDetails
      additionalAddress <- requiredAdditionalAddress
    } yield cdsContact ++ cdsAddress ++ requiredAdditionalContact ++ additionalContact ++ additionalAddress

    fullListOfRequirements.toList.flatten
  }
}
