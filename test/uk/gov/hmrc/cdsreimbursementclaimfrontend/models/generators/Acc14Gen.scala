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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._

object Acc14Gen extends GenUtils {
  implicit val ndrcGen: Gen[NdrcDetails] = gen[NdrcDetails]
}
