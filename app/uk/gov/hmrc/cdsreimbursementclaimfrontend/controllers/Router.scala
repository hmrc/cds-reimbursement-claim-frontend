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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{MrnImporter, ThirdPartyImporter}

trait JourneyTypeRoutes extends Product with Serializable {
  val subKey: Option[String]
}
trait SingleRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = None
}
trait BulkRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = Some("bulk")
}
trait ScheduledRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = Some("scheduled")
}

trait ReferenceNumberTypeRoutes extends Product with Serializable {
  def nextPageForEnterMRN(importer: MrnJourney): Call
}
trait MRNRoutes extends ReferenceNumberTypeRoutes {
  def nextPageForEnterMRN(importer: MrnJourney): Call = importer match {
    case _: MrnImporter        => claimRoutes.CheckDeclarationDetailsController.checkDetails()
    case _: ThirdPartyImporter => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber()
  }
}
trait ERNRoutes extends ReferenceNumberTypeRoutes {
  def nextPageForEnterMRN(importer: MrnJourney): Call =
    claimRoutes.EnterDeclarationDetailsController.enterDeclarationDetails()
}
object ReimbursementRoutes {
  type ReimbursementRoutes = JourneyTypeRoutes with ReferenceNumberTypeRoutes
}

case object DefaultRoutes extends MRNRoutes with SingleRoutes

case object MRNSingleRoutes extends MRNRoutes with SingleRoutes
case object ERNSingleRoutes extends ERNRoutes with SingleRoutes
case object MRNBulkRoutes extends MRNRoutes with BulkRoutes
case object ERNBulkRoutes extends ERNRoutes with BulkRoutes
case object MRNScheduledRoutes extends MRNRoutes with ScheduledRoutes
case object ERNScheduledRoutes extends ERNRoutes with ScheduledRoutes
