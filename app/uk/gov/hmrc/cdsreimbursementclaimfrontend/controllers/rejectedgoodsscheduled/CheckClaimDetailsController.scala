package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

class CheckClaimDetailsController {

}

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{Forms, JourneyControllerComponents, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.CheckClaimDetailsController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.CheckClaimDetailsController.checkClaimDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckClaimDetailsController @Inject() (
val jcc: JourneyControllerComponents,
checkClaimDetailsPage: pages.check-claim-details-scheduled
  )(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
  extends RejectedGoodsScheduledJourneyBaseController {

  val checkClaimDetailsForm: Form[YesNo] = YesOrNoQuestionForm(CheckClaimDetailsController.checkClaimDetailsKey)

  private val postAction: Call = routes.SelectDutyTypesController.submit() //FIXME: routes.CheckClaimDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>

  Ok(checkClaimDetailsPage(checkClaimDetailsForm, postAction)).asFuture

  }

 //val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
//  Future.successful(
//    checkClaimDetailsForm
//  .bindFromRequest()
//  .fold(
//  formWithErrors =>
//  (
//  journey,
//  BadRequest(
//    checkClaimDetailsPage(
//  formWithErrors,
//  postAction
//  )
//  )
//  ),
//    yesNo =>
//  (
//  journey..[SUBMIT METHOD](yesNo),
//Redirect(routes.[NEW CONTROLLER].show()) //FIXME
//  )
//  )
//  )
//}

  }

object CheckClaimDetailsController {
  val checkClaimDetailsKey: String = "check-claim-summary"
}