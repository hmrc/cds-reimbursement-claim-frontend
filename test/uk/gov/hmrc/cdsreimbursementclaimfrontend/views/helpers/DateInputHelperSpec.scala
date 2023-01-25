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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.FormError

class DateInputHelperSpec extends AnyWordSpec with Matchers {

  "DateInputHelper" should {
    "Show error when nothing is provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("error.required"))),
        "day"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when everything that is provided is invalid" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("error.invalid"))),
        "day"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when day is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("day.error.required"))),
        "day"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when month is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("month.error.required"))),
        "month"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when year is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("year.error.required"))),
        "year"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when day and year is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("dayAndYear.error.required"))),
        "day"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when day and month is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("dayAndMonth.error.required"))),
        "day"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when month and year is not provided" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("monthAndYear.error.required"))),
        "month"
      ) shouldBe "some-class govuk-input--error"
    }

    "Show error when the year provided is before 1900" in {
      DateInputHelper.withError(
        "some-class",
        Some(FormError("key", Seq("error.before1900"))),
        "year"
      ) shouldBe "some-class govuk-input--error"
    }

    "Not show error everything is valid" in {
      DateInputHelper.withError(
        "some-class",
        None,
        "day"
      ) shouldBe "some-class"
    }

  }
}
