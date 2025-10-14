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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.i18n.Messages
import play.api.mvc.Call
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.UploadFilesMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.upload_files_description

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class UploadFilesController @Inject() (
  val jcc: ClaimControllerComponents,
  val uploadDocumentsConnector: UploadDocumentsConnector,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  val fileUploadConfig: FileUploadConfig,
  upload_files_description: upload_files_description,
  val featureSwitchService: FeatureSwitchService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledClaimBaseController
    with UploadFilesMixin {

  final val selectDocumentTypePageAction: Call  = routes.ChooseFileTypeController.show
  final val callbackAction: Call                = routes.UploadFilesController.submit
  final def nextPageInClaim(claim: Claim): Call = routes.EnterContactDetailsController.show

  final override def chooseFilesPageDescriptionTemplate: String => Messages => HtmlFormat.Appendable =
    documentType => messages => upload_files_description(documentType)(messages)

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledClaim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final override def modifyClaim(
    claim: Claim,
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, Claim] =
    claim
      .receiveUploadedFiles(
        documentType,
        requestNonce,
        uploadedFiles
      )

}
