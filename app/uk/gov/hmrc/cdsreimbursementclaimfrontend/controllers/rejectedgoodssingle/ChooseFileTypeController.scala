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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import play.api.data.Form
import play.api.mvc.Call
import play.api.mvc.Request
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ChooseFileTypeMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_file_type

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChooseFileTypeController @Inject() (
  val jcc: ClaimControllerComponents,
  chooseFileTypePage: choose_file_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsSingleClaimBaseController
    with ChooseFileTypeMixin {

  final override val uploadFilesRoute: Call =
    routes.UploadFilesController.show

  final override def viewTemplate
    : (Form[Option[UploadDocumentType]], Seq[UploadDocumentType], Boolean) => Request[?] ?=> HtmlFormat.Appendable =
    (form, documentTypes, hasExistingUploads) =>
      chooseFileTypePage(form, documentTypes, hasExistingUploads, routes.ChooseFileTypeController.submit)

  final override def modifyClaim(claim: Claim, documentType: UploadDocumentType): Either[String, Claim] =
    Right(
      claim
        .submitDocumentTypeSelection(documentType)
    )

}
