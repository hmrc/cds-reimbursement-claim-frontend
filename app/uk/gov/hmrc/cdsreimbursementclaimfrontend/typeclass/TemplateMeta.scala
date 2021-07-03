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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, DummyReferenceNumberController, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType.{Bulk, Schedule, Single}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

trait TemplateMeta[C <: FrontendController] {
  def submitUrl: Call
  def getKey(claimType: ClaimType): String
  def nextUrl(claimType: ClaimType, mrn: MovementReferenceNumber): Call
}

object TemplateMeta {

  implicit object DummyControllerClassTemplateMeta extends TemplateMeta[DummyControllerClass] {

    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "bulk-journey"
      case Schedule => "schedule-journey"
      case Single   => "single-journey"
    }

    def submitUrl: Call = routes.DummyControllerClass.testSubmit()

    def nextUrl(claimType: ClaimType, mrn: MovementReferenceNumber): Call = (claimType, mrn) match {
      case (Single, MovementReferenceNumber(Right(MRN(_))))          => routes.NextPageController.nextSinglePage()
      case (Single, MovementReferenceNumber(Left(EntryNumber(_))))   => routes.NextPageController.nextSinglePage()
      case (Bulk, MovementReferenceNumber(Right(MRN(_))))            => routes.NextPageController.nextSinglePage()
      case (Bulk, MovementReferenceNumber(Left(EntryNumber(_))))     => routes.NextPageController.nextSinglePage()
      case (Schedule, MovementReferenceNumber(Right(MRN(_))))        => routes.NextPageController.nextSinglePage()
      case (Schedule, MovementReferenceNumber(Left(EntryNumber(_)))) => routes.NextPageController.nextSinglePage()
    }
  }

  implicit object DummyReferenceNumberControllerTemplateMeta extends TemplateMeta[DummyReferenceNumberController] {

    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "enter-movement-reference-number.bulk"
      case Schedule => "enter-movement-reference-number.schedule"
      case Single   => "enter-movement-reference-number.single"
    }

    def submitUrl: Call = routes.DummyReferenceNumberController.submit()

    def nextUrl(claimType: ClaimType, mrn: MovementReferenceNumber): Call = (claimType, mrn) match {
      case (Single, MovementReferenceNumber(Right(MRN(_))))        => routes.CheckDeclarationDetailsController.checkDetails()
      case (Single, MovementReferenceNumber(Left(EntryNumber(_)))) =>
        routes.EnterDeclarationDetailsController.enterDeclarationDetails()

      case (Bulk, MovementReferenceNumber(Right(MRN(_))))        => routes.CheckDeclarationDetailsController.checkDetails()
      case (Bulk, MovementReferenceNumber(Left(EntryNumber(_)))) =>
        routes.EnterDeclarationDetailsController.enterDeclarationDetails()

      case (Schedule, MovementReferenceNumber(Right(MRN(_))))        => routes.CheckDeclarationDetailsController.checkDetails()
      case (Schedule, MovementReferenceNumber(Left(EntryNumber(_)))) => baseRoutes.IneligibleController.ineligible()
    }
  }

  object syntax {

    implicit class ClaimTypeTemplateMetaOps(val claimType: ClaimType) extends AnyVal {
      def showPage[T <: FrontendController](
        f: (String, Call) => Result
      )(implicit templateMeta: TemplateMeta[T]): Result = {
        val key = templateMeta.getKey(claimType)
        val url = templateMeta.submitUrl
        f(key, url)
      }
    }
  }
}
