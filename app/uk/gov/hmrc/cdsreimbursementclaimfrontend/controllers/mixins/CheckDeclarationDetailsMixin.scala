/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc.*
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.CommonClaimProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

import scala.concurrent.Future

trait CheckDeclarationDetailsMixin extends ClaimBaseController {

  type Claim <: claims.Claim & ClaimBase & CommonClaimProperties

  def getDisplayDeclaration(claim: Claim): Option[DisplayDeclaration]
  def continueRoute(claim: Claim): Call
  val enterMovementReferenceNumberRoute: Call

  def viewTemplate: (DisplayDeclaration, Claim) => Request[?] ?=> HtmlFormat.Appendable

  final val show: Action[AnyContent] = actionReadClaim { claim =>
    Future.successful(
      getDisplayDeclaration(claim).fold(Redirect(baseRoutes.IneligibleController.ineligible))(declaration =>
        Ok(
          viewTemplate(
            declaration,
            claim
          )
        )
      )
    )
  }

  final val submit: Action[AnyContent] = simpleActionReadClaim { claim =>
    Redirect(continueRoute(claim))
  }
}
