package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, JourneyControllerComponents, SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckDeclarationDetailsController @Inject()(
                                                   val authenticatedAction: AuthenticatedAction,
                                                   val sessionDataAction: SessionDataAction,
                                                   val sessionStore: SessionCache,
                                                   val jcc: JourneyControllerComponents,
                                                   claimService: ClaimService,
                                                   checkDeclarationDetailsPage: pages.check_declaration_details
                                                 )(implicit viewConfig: ViewConfig, ec: ExecutionContext) extends RejectedGoodsSingleJourneyBaseController {

def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
  val declaration: DisplayDeclaration = ???
  val postAction: Call = ???

  Ok(checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate = false, postAction))

}

}
