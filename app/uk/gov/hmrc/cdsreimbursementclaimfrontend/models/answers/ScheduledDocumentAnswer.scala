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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.data.Validated
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.{MissingAnswerError, Validator}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument

final case class ScheduledDocumentAnswer(uploadDocument: UploadDocument) extends AnyVal

object ScheduledDocumentAnswer {

  val validator: Validator[Option, ScheduledDocumentAnswer] = maybeScheduledDocument =>
    Validated.condNel(
      maybeScheduledDocument.isDefined,
      maybeScheduledDocument,
      MissingAnswerError("Scheduled document")
    )

  implicit val scheduledDocumentFormat: OFormat[ScheduledDocumentAnswer] =
    Json.format[ScheduledDocumentAnswer]
}
