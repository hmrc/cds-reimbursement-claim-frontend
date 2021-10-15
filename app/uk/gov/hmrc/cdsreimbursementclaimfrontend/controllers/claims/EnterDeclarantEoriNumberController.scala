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

import cats.instances.future.catsStdInstancesForFuture
import play.api.data.Form
import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import play.api.data.Forms.{mapping, text}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  enterDeclarantEoriNumberPage: pages.enter_declarant_eori_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[DeclarantEoriNumber] = _.declarantEoriNumberAnswer

  def enterDeclarantEoriNumber(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantEoriNumber] { (_, answers, router) =>
        val emptyForm                             = EnterDeclarantEoriNumberController.eoriNumberForm
        val filledForm: Form[DeclarantEoriNumber] = answers.fold(emptyForm)(emptyForm.fill)
        Ok(enterDeclarantEoriNumberPage(filledForm, router))
      }
    }

  def enterDeclarantEoriNumberSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantEoriNumber] { (fillingOutClaim, _, router) =>
        EnterDeclarantEoriNumberController.eoriNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDeclarantEoriNumberPage(
                  requestFormWithErrors,
                  router
                )
              ),
            declarantEoriNumber => {
              val newDraftClaim =
                fillingOutClaim.draftClaim.fold(_.copy(declarantEoriNumberAnswer = Some(declarantEoriNumber)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get declarant eori number"),
                _ =>
                  hasMatchOnEori(fillingOutClaim, declarantEoriNumber) match {
                    case Left(e)  =>
                      logger.warn("could not get data to determine third party flow", e)
                      Redirect(baseRoutes.IneligibleController.ineligible())
                    case Right(b) =>
                      if (b) {
                        Redirect(routes.CheckDeclarationDetailsController.show(journey))
                      } else {
                        logger.warn("could not match Eoris for third party flow")
                        Redirect(baseRoutes.IneligibleController.ineligible())
                      }
                  }
              )
            }
          )
      }
    }

  private def hasMatchOnEori(
    fillingOutClaim: FillingOutClaim,
    declarantEoriNumber: DeclarantEoriNumber
  ): Either[Error, Boolean] = {
    val maybeImporterEoriNumber: Option[ImporterEoriNumber] =
      fillingOutClaim.draftClaim.fold(_.importerEoriNumberAnswer)

    val maybeDisplayDeclaration: Option[DisplayDeclaration] = fillingOutClaim.draftClaim.fold(_.displayDeclaration)

    (maybeDisplayDeclaration, maybeImporterEoriNumber, Some(declarantEoriNumber)) match {
      case (Some(displayDeclaration), Some(importerEoriNumber), Some(declarationEori)) =>
        displayDeclaration.displayResponseDetail.consigneeDetails match {
          case Some(consigneeDetails) =>
            if (
              (consigneeDetails.consigneeEORI === importerEoriNumber.value.value) && (declarationEori.value.value === displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI)
            ) {
              Right(true)
            } else Right(false)
          case None                   => Left(Error("could not retrieve consignee eori"))

        }
      case _                                                                           => Left(Error("could not retrieve details to determine third party flow"))
    }
  }

}

object EnterDeclarantEoriNumberController {

  val eoriNumberMapping: Mapping[Eori] =
    text(maxLength = 18)
      .verifying("invalid.number", str => Eori.isValid(str))
      .transform[Eori](str => Eori(str), eori => eori.value)

  val eoriNumberForm: Form[DeclarantEoriNumber] = Form(
    mapping(
      "enter-declarant-eori-number" -> eoriNumberMapping
    )(DeclarantEoriNumber.apply)(DeclarantEoriNumber.unapply)
  )

}
