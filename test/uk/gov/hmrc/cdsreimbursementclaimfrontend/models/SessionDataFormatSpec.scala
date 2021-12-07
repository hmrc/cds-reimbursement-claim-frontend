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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import org.scalatest.Ignore

@Ignore
class SessionDataFormatSpec extends AnyWordSpec with JsonFormatTest with Matchers with ScalaCheckPropertyChecks {

  "JourneyStatus" should {
    "be serializable into a JSON format" in {
      forAll { journeyStatus: JourneyStatus =>
        validateCanReadAndWriteJson(journeyStatus)
      }
    }
  }

  "SessionData" should {
    "be serializable into a JSON format" in {
      forAll { sessionData: SessionData =>
        validateCanReadAndWriteJson(sessionData)
      }
    }
  }

}
