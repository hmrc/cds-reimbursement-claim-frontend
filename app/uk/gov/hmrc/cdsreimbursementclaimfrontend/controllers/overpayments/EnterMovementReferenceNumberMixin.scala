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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments

import cats.syntax.all._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

trait EnterMovementReferenceNumberMixin {
  self: SessionDataExtractor with SessionUpdates =>

  def updateMRN(fillingOutClaim: FillingOutClaim, mrn: MRN): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.copy(movementReferenceNumber = Some(mrn))
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN,
    acc14: DisplayDeclaration,
    maybeContactDetails: Option[MrnContactDetails],
    maybeContactAddress: Option[ContactAddress]
  ): SessionDataTransform = {

    val updatedDraftClaim = fillingOutClaim.draftClaim.copy(
      movementReferenceNumber = Some(mrn),
      displayDeclaration = Some(acc14),
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddress,
      associatedMRNsAnswer = None,
      associatedMRNsDeclarationAnswer = None
    )

    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def renewMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN,
    acc14: DisplayDeclaration,
    maybeContactDetails: Option[MrnContactDetails],
    maybeContactAddress: Option[ContactAddress]
  ): SessionDataTransform = {

    val typeOfClaimAnswer: TypeOfClaimAnswer = getTypeOfClaim(fillingOutClaim.draftClaim)
    val blankDraftClaim                      = DraftClaim.blank
    val updatedDraftClaim                    = DraftClaim.blank.copy(
      typeOfClaim = Some(typeOfClaimAnswer),
      movementReferenceNumber = Some(mrn),
      displayDeclaration = Some(acc14),
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddress,
      associatedMRNsAnswer = None,
      associatedMRNsDeclarationAnswer = None
    )
    val journey                              = FillingOutClaim(fillingOutClaim.ggCredId, fillingOutClaim.signedInUserDetails, blankDraftClaim)

    updateDraftClaim(journey, updatedDraftClaim)
  }

  def updateDraftClaim(fillingOutClaim: FillingOutClaim, newDraftClaim: DraftClaim): SessionDataTransform = {
    val update: SessionDataTransform = _.copy(journeyStatus = Some(fillingOutClaim.copy(draftClaim = newDraftClaim)))
    update
  }

  def evaluateMrnJourneyFlow(
    signedInUserDetails: SignedInUserDetails,
    maybeDisplayDeclaration: Option[DisplayDeclaration]
  ): Either[Error, MrnJourney] =
    maybeDisplayDeclaration match {
      case Some(displayDeclaration) =>
        (
          displayDeclaration.displayResponseDetail.consigneeDetails,
          Some(displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI)
        ) match {
          case (None, _)                                        => Right(MrnJourney.ThirdPartyImporter(displayDeclaration))
          case (Some(consigneeDetails), Some(declarantDetails)) =>
            if (consigneeDetails.consigneeEORI === signedInUserDetails.eori.value)
              Right(MrnJourney.MrnImporter(displayDeclaration))
            else if (
              consigneeDetails.consigneeEORI =!= signedInUserDetails.eori.value || declarantDetails =!= signedInUserDetails.eori.value
            )
              Right(MrnJourney.ThirdPartyImporter(displayDeclaration))
            else
              Right(MrnJourney.ThirdPartyImporter(displayDeclaration))
          case _                                                =>
            Left(Error("could not determine if signed in user's Eori matches any on the declaration"))
        }

      case None => Left(Error("received no declaration information"))
    }

  def extractContactDetails(
    displayDeclaration: DisplayDeclaration,
    mrnJourney: MrnJourney,
    email: Email
  ): Option[MrnContactDetails] =
    mrnJourney match {
      case MrnImporter(_) =>
        val consignee  = displayDeclaration.displayResponseDetail.consigneeDetails
        val maybeName  = consignee.flatMap(_.contactDetails).flatMap(_.contactName)
        val maybePhone = consignee.flatMap(_.contactDetails).flatMap(_.telephone)
        maybeName.map(name => MrnContactDetails(name, email, maybePhone.map(PhoneNumber(_))))
      case _              =>
        val declarant  = displayDeclaration.displayResponseDetail.declarantDetails
        val maybeName  = declarant.contactDetails.flatMap(_.contactName)
        val maybePhone = declarant.contactDetails.flatMap(_.telephone)
        maybeName.map(name => MrnContactDetails(name, email, maybePhone.map(PhoneNumber(_))))
    }

  def extractContactAddress(displayDeclaration: DisplayDeclaration, mrnJourney: MrnJourney): Option[ContactAddress] =
    (mrnJourney match {
      case MrnImporter(_) =>
        val consignee = displayDeclaration.displayResponseDetail.consigneeDetails
        consignee.flatMap(_.contactDetails)
      case _              =>
        val declarant = displayDeclaration.displayResponseDetail.declarantDetails
        declarant.contactDetails
    }).flatMap { contactDetails =>
      (contactDetails.addressLine1, contactDetails.postalCode).mapN { (addressLine1, postCode) =>
        ContactAddress(
          addressLine1,
          contactDetails.addressLine2,
          None,
          contactDetails.addressLine3.getOrElse(""),
          postCode,
          contactDetails.countryCode.map(Country(_)).getOrElse(Country.uk)
        )
      }
    }

}
