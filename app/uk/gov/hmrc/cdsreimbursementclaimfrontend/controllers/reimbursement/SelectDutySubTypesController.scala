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

///*
// * Copyright 2021 HM Revenue & Customs
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement
//
//import cats.data.EitherT
//import com.google.inject.Inject
//import play.api.Configuration
//import play.api.mvc.MessagesControllerComponents
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DutyPaidAndClaimAmountAnswer, DutySubTypesSelectedAnswer}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
//import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
//
//import scala.concurrent.{ExecutionContext, Future}
//
//class SelectDutySubTypesController @Inject() (
//  val authenticatedAction: AuthenticatedAction,
//  val sessionDataAction: SessionDataAction,
//  val sessionCache: SessionCache,
//  cc: MessagesControllerComponents,
//  val config: Configuration
//  //selectDutySubTypesPage: pages.select_duty_sub_types
//)(implicit ec: ExecutionContext)
//    extends FrontendController(cc)
//    with WithAuthAndSessionDataAction
//    with Logging
//    with SessionDataExtractor
//    with SessionUpdates {
//
//  implicit val dataExtractor: DraftC285Claim => DutySubTypesSelectedAnswer = _.dutySubTypesSelectedAnswer
//
//  def start() = authenticatedActionWithSessionData.async { implicit request =>
//    withAnswers[DutySubTypesSelectedAnswer] { (fillingOutClaim, answer) =>
//      /*
//      Get the latest duty types selection
//      Compare this to the keySet in the DutySubTypesSelectedAnswer
//      If same then redirect to showDutySubTypes
//      If different and then are less duty types, find out which ones have been de-selected and remove those from the Map and the corressponding claims amounts model and update session and
//      then redirect to showDutySubTypes
//       */
//
//      val maybeDutyTypesSelectedAnswer                               = fillingOutClaim.draftClaim.fold(_.dutyTypesSelectedAnswer)
//      val dutyPaidAndClaimAmountAnswer: DutyPaidAndClaimAmountAnswer =
//        fillingOutClaim.draftClaim.fold(_.dutyPaidAndClaimAmountAnswer)
//
//      answer.fold(
//        Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))
//      ) { dutySubTypesSelectedAnswer =>
//        val updatedReimbursementState =
//          SelectDutySubTypesController.computeReimbursementState(fillingOutClaim, dutySubTypesSelectedAnswer)
//        val foo: DraftC285Claim       =
//          fillingOutClaim.draftClaim.fold(
//            _.copy(
//              dutyTypesSelectedAnswer = Some(updatedReimbursementState.dutyTypesSelectedAnswer),
//              dutySubTypesSelectedAnswer = Some(updatedReimbursementState.dutySubTypesSelectedAnswer),
//              dutyPaidAndClaimAmountAnswer = Some(updatedReimbursementState.dutyPaidAndClaimAmount)
//            )
//          )
//
//        val result: EitherT[Future, models.Error, Unit] = for {
//          _ <- EitherT(
//                 updateSession(sessionCache, request)(
//                   _.copy(
//                     journeyStatus = Some(
//                       FillingOutClaim(fillingOutClaim.ggCredId, fillingOutClaim.signedInUserDetails, foo)
//                     )
//                   )
//                 )
//               )
//        } yield ()
//
//        result.fold(
//          _ => Ok(""),
//          _ =>
//            Redirect(
//              reimbursementRoutes.SelectDutySubTypesController.start() //TODO: fix
//            )
//        )
//
//      }
//
//    }
//  }
//
//  def showDutySubTypes(dutyType: DutyType) = Action {
//    Ok(s"show ${dutyType.toString}")
//  }
//
//  def submitShowDutySubTypes(dutyType: DutyType) = Action {
//    Ok(s"submitted: ${dutyType.toString}")
//  }
//
//}
////
////object SelectDutySubTypesController {
////
////  def computeReimbursementState(
////    maybeDutyTypesSelectedAnswer: Option[DutyTypesSelectedAnswer],
////    dutySubTypesSelectedAnswer: DutySubTypesSelectedAnswer,
////    maybeDutyPaidAndClaimAmountAnswer: Option[DutyPaidAndClaimAmount]
////  ): UpdatedReimbursementState =
////    UpdatedReimbursementState(
////      maybeDutyTypesSelectedAnswer.getOrElse(DutyTypesSelectedAnswer(DutyType.UkDuty)),
////      dutySubTypesSelectedAnswer,
////      maybeDutyPaidAndClaimAmountAnswer.getOrElse(DutyPaidAndClaimAmount(""))
////    )
////  //    (maybeDutyTypesSelectedAnswer, dutySubTypesSelectedAnswer, maybeDutyPaidAndClaimAmountAnswer) {
//////      case (Some(dutyTypesSelectedAnswer), dutySubTypesSelectedAnswer, Some(dutyPaidAndClaimAmount)) =>
//////        maybeDutyTypesSelectedAnswer match {
//////          case Some(dutyTypesSelectedAnswer) =>
//////            val k: Set[DutyType] = dutySubTypesSelectedAnswer.keySet
//////            val a: Set[DutyType] = dutyTypesSelectedAnswer.toList.toSet
//////            if (a === k) { //same size and content
//////              Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
//////            } else {
//////              if (a.size < k.size) {
//////                // return updated dutySubTypesSelectedAnswer and also the EnterDutyPaidAndClaim
//////                Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
//////              } else if (a.size === k.size) { //same size but different content
//////                Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
//////              } else {
//////                Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
//////              }
//////            }
//////
//////          case None => Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes())
//////        }
//////        None
//////      case (Some(dutyTypesSelectedAnswer), dutySubTypesSelectedAnswer, None)                         => None
//////      case (None, _, _)                                                                              => None
//////    }
////
////}
