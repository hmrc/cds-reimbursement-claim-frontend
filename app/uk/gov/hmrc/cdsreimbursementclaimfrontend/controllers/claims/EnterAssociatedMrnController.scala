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
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.FormError
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMrnController.enterAssociatedMrnKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterAssociatedMrnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  claimService: ClaimService,
  enterAssociatedMrnPage: pages.enter_associated_mrn,
  mrnDoesNotExistPage: pages.mrn_does_not_exist
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  import EnterAssociatedMrnController._

  implicit val dataExtractor: DraftC285Claim => Option[AssociatedMRNsAnswer] = _.associatedMRNsAnswer

  def mrnInputForm(existing: List[MRN] = Nil): Form[AssociatedMrn] =
    Form(
      mapping(
        enterAssociatedMrnKey -> nonEmptyText
          .verifying("invalid.number", str => str.isEmpty || MRN.isValid(str))
          .verifying("error.exists", mrn => existing.forall(_.value =!= mrn))
          .transform[AssociatedMrn](MRN(_), _.value)
      )(identity)(Some(_))
    )

  def changeMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    enterMrn(index)

  def enterMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (_, associatedMRNsAnswer) =>
        associatedMRNsAnswer
          .get(index)
          .fold {
            if (associatedMRNsAnswer.canAppendAt(index))
              Ok(enterAssociatedMrnPage(index, mrnInputForm(), editing = false))
            else
              BadRequest(mrnDoesNotExistPage())
          } { mrn =>
            Ok(enterAssociatedMrnPage(index, mrnInputForm().fill(mrn), editing = true))
          }
      }
    }

  def submitChangedMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    submitEnteredMrn(index)

  def submitEnteredMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (fillingOutClaim, associatedMRNsAnswer) =>
        val form = mrnInputForm(associatedMRNsAnswer.listAllBut(index))

        def displayInputError(mrn: MRN, errorMessageKey: String) = {
          val editing: Boolean = associatedMRNsAnswer.isDefinedAt(index)
          BadRequest(
            enterAssociatedMrnPage(index, form.fill(mrn).withError(FormError("", Seq(errorMessageKey))), editing)
          )
        }

        def replaceOrAddMrn(mrn: AssociatedMrn): EitherT[Future, Result, Unit] =
          for {
            declaration <-
              claimService
                .getDisplayDeclaration(mrn)
                .leftMap(logAndDisplayError(s"Error getting declaration"))
                .transform {
                  case Right(None)    => Left(displayInputError(mrn, ""))
                  case Right(Some(d)) =>
                    println(d)
                    Right(d)
                  case Left(r)        => Left(r)
                }

            _ = EitherT.fromEither[Future] {
                  canAcceptAssociatedDeclaration(fillingOutClaim, declaration) match {
                    case false => Left(displayInputError(mrn, ""))
                    case true  => Right(())
                  }
                }

            updatedDraftClaim <-
              EitherT.fromEither[Future](
                updateAssociatedMrn(index, fillingOutClaim, mrn, declaration).left
                  .map(_ => displayInputError(mrn, ""))
              )

            _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedDraftClaim.some)))
                   .leftMap(logAndDisplayError(s"Error storing ${index.ordinalNumeral} MRN: "))

          } yield ()

        form
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterAssociatedMrnPage(index, formWithErrors)),
            replaceOrAddMrn(_)
              .fold(identity, _ => Redirect(routes.CheckMovementReferenceNumbersController.showMrns()))
          )
      }
    }

}

object EnterAssociatedMrnController {

  val enterAssociatedMrnKey: String = "enter-associated-mrn"

  def canAcceptAssociatedDeclaration(
    fillingOutClaim: FillingOutClaim,
    displayDeclaration: DisplayDeclaration
  ): Boolean = {
    val userEori: String                     =
      fillingOutClaim.signedInUserDetails.eori.value
    val leadConsigneeEORIOpt: Option[String] = fillingOutClaim.consigneeEORI
    val leadDeclarantEORIOpt: Option[String] = fillingOutClaim.declarantEORI
    val consigneeEORIOpt: Option[String]     =
      displayDeclaration.displayResponseDetail.consigneeDetails.map(_.consigneeEORI)
    val declarantEORI: String                =
      displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI

    declarantEORI === userEori &&
    consigneeEORIOpt.contains(userEori) &&
    leadDeclarantEORIOpt.contains(
      declarantEORI
    ) && leadConsigneeEORIOpt.exists(consigneeEORIOpt.contains(_))
  }

  def updateAssociatedMrn(
    index: AssociatedMrnIndex,
    fillingOutClaim: FillingOutClaim,
    mrn: AssociatedMrn,
    declaration: DisplayDeclaration
  ): Either[Unit, FillingOutClaim] =
    FillingOutClaim.ofEither(fillingOutClaim) { draftClaim =>
      for {
        associatedMRNsAnswer            <- draftClaim.associatedMRNsAnswer.replaceOrAppend(index, mrn)
        associatedMRNsDeclarationAnswer <-
          draftClaim.associatedMRNsDeclarationAnswer.replaceOrAppend(index, declaration)
      } yield draftClaim.copy(
        associatedMRNsAnswer = associatedMRNsAnswer,
        associatedMRNsDeclarationAnswer = associatedMRNsDeclarationAnswer
      )
    }
}
