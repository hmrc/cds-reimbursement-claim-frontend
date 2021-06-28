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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass

import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.Journey.{BulkJourney, SingleJourney}

trait TemplateContent[T] {
  val key: String
}

object TemplateContent {

  implicit val singleJourneyTemplateContent: TemplateContent[SingleJourney] = new TemplateContent[SingleJourney] {
    val key: String = "single-journey"
  }

  implicit val bulkJourneyTemplateContent: TemplateContent[BulkJourney] = new TemplateContent[BulkJourney] {
    val key: String = "bulk-journey"
  }
}
