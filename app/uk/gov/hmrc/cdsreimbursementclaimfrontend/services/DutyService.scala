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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import com.google.inject.{ImplementedBy, Inject}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.UKTaxCode

import javax.inject.Singleton

@ImplementedBy(classOf[DefaultDutyService])
trait DutyService {
  def getListOfUKDuties: List[UKTaxCode]
}

@Singleton
class DefaultDutyService @Inject() () extends DutyService {

  override def getListOfUKDuties: List[UKTaxCode] = List(
    TaxCode.UKTaxCode.A00,
    TaxCode.UKTaxCode.A20,
    TaxCode.UKTaxCode.A30,
    TaxCode.UKTaxCode.A35,
    TaxCode.UKTaxCode.A40,
    TaxCode.UKTaxCode.A45,
    TaxCode.UKTaxCode.B00
  )

}
