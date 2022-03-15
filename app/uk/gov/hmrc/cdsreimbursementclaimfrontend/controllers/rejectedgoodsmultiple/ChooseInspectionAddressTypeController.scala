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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.inspectionAddressTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.AddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import scala.concurrent.ExecutionContext

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  inspectionAddressPage: pages.choose_inspection_address_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController
    with AddressLookupMixin[RejectedGoodsMultipleJourney] {

  val postAction: Call                      = routes.ChooseInspectionAddressTypeController.submit()
  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show()
  override val retrieveLookupAddress: Call  = routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    populateAddresses(journey) match {
      case List()    =>
        Redirect(routes.ChooseInspectionAddressTypeController.redirectToALF()).asFuture
      case addresses =>
        Ok(
          inspectionAddressPage(
            addresses,
            inspectionAddressTypeForm.withDefault(journey.getInspectionAddressType),
            postAction
          )
        ).asFuture
    }
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      inspectionAddressTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(inspectionAddressPage(populateAddresses(journey), formWithErrors, postAction))
            ),
          {
            case Other                 =>
              (
                journey,
                Redirect(routes.ChooseInspectionAddressTypeController.redirectToALF())
              )
            case inspectionAddressType =>
              inspectionAddressForType(inspectionAddressType, journey)
                .map { address =>
                  redirectToTheNextPage(journey.submitInspectionAddress(address))
                }
                .getOrElse((journey, Ok("Where do we go if this fails?")))
          }
        )
        .asFuture
    },
    fastForwardToCYAEnabled = false
  )

  private def inspectionAddressForType(
    addressType: InspectionAddressType,
    journey: RejectedGoodsMultipleJourney
  ): Option[InspectionAddress] =
    addressType match {
      case Importer  => journey.getConsigneeContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
      case Declarant =>
        journey.getDeclarantContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
    }

  private def populateAddresses(journey: RejectedGoodsMultipleJourney) = Seq(
    journey.getConsigneeContactDetailsFromACC14.flatMap(_.showAddress).map(Importer  -> _),
    journey.getDeclarantContactDetailsFromACC14.flatMap(_.showAddress).map(Declarant -> _)
  ).flatten(Option.option2Iterable)

  private val nextPage: Call = routes.CheckBankDetailsController.show()

  override def update(journey: RejectedGoodsMultipleJourney): ContactAddress => RejectedGoodsMultipleJourney = {
    contactAddress =>
      journey.submitInspectionAddress(InspectionAddress.ofType(Other).mapFrom(contactAddress))
  }

  override def redirectToTheNextPage(journey: RejectedGoodsMultipleJourney): (RejectedGoodsMultipleJourney, Result) =
    (
      journey,
      if (journey.hasCompleteAnswers)
        Redirect(checkYourAnswers)
      else
        Redirect(nextPage)
    )
}
