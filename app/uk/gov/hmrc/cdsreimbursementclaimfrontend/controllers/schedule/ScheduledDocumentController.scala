package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.schedule

import com.google.inject.{Inject, Singleton}
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload.{FileUploadController, FileUploadServices}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.ScheduledDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService

import scala.concurrent.ExecutionContext

@Singleton
class ScheduledDocumentController @Inject() (
  authenticatedAction: AuthenticatedAction,
  sessionDataAction: SessionDataAction,
  upscanService: UpscanService,
  fileUploadServices: FileUploadServices,
  errorHandler: ErrorHandler
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, sessionStore: SessionCache)
  extends FileUploadController(authenticatedAction, sessionDataAction, upscanService, errorHandler, cc)
    with SessionDataExtractor {

  import fileUploadServices._

  implicit val supportingEvidenceExtractor: DraftC285Claim => Option[ScheduledDocument] =
    _.scheduledDocumentAnswer
}
