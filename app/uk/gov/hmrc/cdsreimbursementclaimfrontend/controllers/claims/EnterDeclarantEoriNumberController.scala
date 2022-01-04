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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.instances.future.catsStdInstancesForFuture
import play.api.data.Form
import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import javax.inject.Inject
import javax.inject.Singleton
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
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

  val eoriNumberFormKey: String = "enter-declarant-eori-number"

  implicit val dataExtractor: DraftClaim => Option[DeclarantEoriNumberAnswer] = _.declarantEoriNumberAnswer

  def enterDeclarantEoriNumber(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantEoriNumberAnswer] { (_, answers, router) =>
        val emptyForm              = eoriNumberForm(eoriNumberFormKey)
        val filledForm: Form[Eori] =
          answers.fold(emptyForm)(declarantEoriNumberAnswer => emptyForm.fill(declarantEoriNumberAnswer.value))
        Ok(enterDeclarantEoriNumberPage(filledForm, router.submitUrlForEnterDeclarantEoriNumber()))
      }
    }

  def enterDeclarantEoriNumberSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantEoriNumberAnswer] { (fillingOutClaim, _, router) =>
        eoriNumberForm(eoriNumberFormKey)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDeclarantEoriNumberPage(
                  requestFormWithErrors,
                  router.submitUrlForEnterDeclarantEoriNumber()
                )
              ),
            declarantEoriNumber => {

              val updatedJourney =
                FillingOutClaim.from(fillingOutClaim)(
                  _.copy(declarantEoriNumberAnswer = Some(DeclarantEoriNumberAnswer(declarantEoriNumber)))
                )

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get declarant eori number"),
                _ =>
                  hasMatchOnEori(fillingOutClaim, DeclarantEoriNumberAnswer(declarantEoriNumber)) match {
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
    declarantEoriNumber: DeclarantEoriNumberAnswer
  ): Either[Error, Boolean] = {
    val maybeImporterEoriNumber: Option[ImporterEoriNumberAnswer] =
      fillingOutClaim.draftClaim.importerEoriNumberAnswer

    val maybeDisplayDeclaration: Option[DisplayDeclaration] = fillingOutClaim.draftClaim.displayDeclaration

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
