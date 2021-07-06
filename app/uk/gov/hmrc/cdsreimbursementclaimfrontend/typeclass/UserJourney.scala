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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass

import play.api.mvc.{Call, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, DummyReferenceNumberController, SelectBasisForClaimController, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, MovementReferenceNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType.{Bulk, Schedule, Single}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

final case class JourneyParameters(claimType: ClaimType, mrn: MovementReferenceNumber)

trait UserJourney[C <: FrontendController, A] {
  def submitUrl: Call
  def getKey(claimType: ClaimType): String
  def nextUrl(a: A): Call
}

object UserJourney {

  implicit object SelectBasisForClaimControllerTemplateMeta extends UserJourney[SelectBasisForClaimController, BasisOfClaim] {

    override def submitUrl: Call = ???

    override def getKey(claimType: ClaimType): String = ???

    override def nextUrl(basisOfClaim: BasisOfClaim): Call = basisOfClaim match {
      case BasisOfClaim.DuplicateEntry =>
        routes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn()
      case _ =>
        routes.EnterCommoditiesDetailsController.enterCommoditiesDetails()
    }
  }

  implicit object DummyReferenceNumberControllerTemplateMeta extends UserJourney[DummyReferenceNumberController, JourneyParameters] {

    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "enter-movement-reference-number.bulk"
      case Schedule => "enter-movement-reference-number.schedule"
      case Single   => "enter-movement-reference-number.single"
    }

    def submitUrl: Call = routes.DummyReferenceNumberController.submit()

    override def nextUrl(params: JourneyParameters): Call = (params.claimType, params.mrn) match {
      case (Single, MovementReferenceNumber(Right(MRN(_)))) =>
        routes.SelectWhoIsMakingTheClaimController.selectDeclarantType()
      case (Single, MovementReferenceNumber(Left(EntryNumber(_)))) =>
        routes.EnterDeclarationDetailsController.enterDeclarationDetails()

      case (Bulk, MovementReferenceNumber(Right(MRN(_)))) => routes.DummyScheduleUploadController.show()
      case (Bulk, MovementReferenceNumber(Left(EntryNumber(_)))) =>
        routes.EnterDeclarationDetailsController.enterDeclarationDetails()

      case (Schedule, MovementReferenceNumber(Right(MRN(_)))) => routes.DummyScheduleUploadController.show()
      case (Schedule, MovementReferenceNumber(Left(EntryNumber(_)))) => baseRoutes.IneligibleController.ineligible()
    }
  }

  implicit object DummyControllerClassTemplateMeta extends UserJourney[DummyControllerClass, JourneyParameters] {

    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "bulk-journey"
      case Schedule => "schedule-journey"
      case Single   => "single-journey"
    }

    def submitUrl: Call = routes.DummyControllerClass.testSubmit()

    def nextUrl(params: JourneyParameters): Call = (params.claimType, params.mrn) match {
      case (Single, MovementReferenceNumber(Right(MRN(_))))          => routes.NextPageController.nextSinglePage()
      case (Single, MovementReferenceNumber(Left(EntryNumber(_))))   => routes.NextPageController.nextSinglePage()
      case (Bulk, MovementReferenceNumber(Right(MRN(_))))            => routes.NextPageController.nextBulkPage()
      case (Bulk, MovementReferenceNumber(Left(EntryNumber(_))))     => routes.NextPageController.nextBulkPage()
      case (Schedule, MovementReferenceNumber(Right(MRN(_))))        => routes.NextPageController.nextBulkPage()
      case (Schedule, MovementReferenceNumber(Left(EntryNumber(_)))) => routes.NextPageController.nextBulkPage()
    }
  }

  object syntax {

    implicit class ClaimTypeTemplateMetaOps(val claimType: ClaimType) extends AnyVal {
      def showPage[T <: FrontendController, A](
        f: (String, Call) => Result
      )(implicit templateMeta: UserJourney[T, A]): Result = {
        val key = templateMeta.getKey(claimType)
        val url = templateMeta.submitUrl
        f(key, url)
      }
    }
  }
}
