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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory

object DutyTypeGen {

  lazy val genDuty: Gen[DutyType]                 = Gen.oneOf(DutyTypes.all)
  lazy val genCustomsDuty: Gen[DutyType]          = Gen.oneOf(DutyTypes.custom)
  lazy val genExciseDuty: Gen[DutyType]           = Gen.oneOf(DutyTypes.excise)
  lazy val genExciseCategory: Gen[ExciseCategory] = Gen.oneOf(ExciseCategory.all)

  lazy val genDuties: Gen[List[DutyType]] = Gen.listOf(genDuty).map(_.distinct)

  implicit lazy val arbitraryDutyTypeGen: Arbitrary[DutyType] = Arbitrary(genDuty)

  implicit lazy val arbitraryDutyTypes: Arbitrary[List[DutyType]] = Arbitrary(genDuties)
}
