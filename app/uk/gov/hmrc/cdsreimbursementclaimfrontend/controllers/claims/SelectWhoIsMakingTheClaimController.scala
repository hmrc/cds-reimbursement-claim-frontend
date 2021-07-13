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

import cats.data.EitherT
import cats.syntax.all._
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectWhoIsMakingTheClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  selectWhoIsMakingTheClaimPage: pages.select_who_is_making_the_claim
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {


}

object SelectWhoIsMakingTheClaimController {

  sealed trait DeclarantType extends Product with Serializable {
    def repr: String
  }

  object DeclarantType {
    final case object Importer extends DeclarantType {
      override def repr = "Importer"
    }
    final case object AssociatedWithImporterCompany extends DeclarantType {
      override def repr = "Associated with Importer Company"
    }
    final case object AssociatedWithRepresentativeCompany extends DeclarantType {
      override def repr = "Associated Representative Company"
    }

    implicit val format: OFormat[DeclarantType] = derived.oformat[DeclarantType]()
  }

  val chooseDeclarantTypeForm: Form[DeclarantType] =
    Form(
      mapping(
        "select-who-is-making-the-claim" -> number
          .verifying("invalid", a => a === 0 || a === 1 || a === 2)
          .transform[DeclarantType](
            value =>
              if (value === 0) DeclarantType.Importer
              else if (value === 1) DeclarantType.AssociatedWithImporterCompany
              else DeclarantType.AssociatedWithRepresentativeCompany,
            {
              case DeclarantType.Importer                            => 0
              case DeclarantType.AssociatedWithImporterCompany       => 1
              case DeclarantType.AssociatedWithRepresentativeCompany => 2
            }
          )
      )(identity)(Some(_))
    )
}
