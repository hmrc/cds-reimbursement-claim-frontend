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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait TaxCode extends Product with Serializable

object TaxCode {
  sealed trait UKTaxCode extends TaxCode with Product with Serializable {
    def description: String
  }

  object UKTaxCode {
    case object A00 extends UKTaxCode {
      override def description: String = "Customs Duty"
    }

    case object A20 extends UKTaxCode {
      override def description: String = "Additional Duty"
    }

    case object A30 extends UKTaxCode {
      override def description: String = "Definitive Anti-Dumping Duty"
    }

    case object A35 extends UKTaxCode {
      override def description: String = "Provisional Anti-Dumping Duty"
    }

    case object A40 extends UKTaxCode {
      override def description: String = "Definitive Countervailing Duty"
    }

    case object A45 extends UKTaxCode {
      override def description: String = "Provisional Countervailing Duty"
    }

    case object B00 extends UKTaxCode {
      override def description: String = "Value Added Tax"
    }
  }

  sealed trait EUTaxCode extends TaxCode with Product with Serializable {
    def description: String
  }

  object EUTaxCode {
    case object A50 extends EUTaxCode {
      override def description: String = "Customs Duty"
    }
    case object A70 extends EUTaxCode {
      override def description: String = "Additional Duty"
    }
    case object A80 extends EUTaxCode {
      override def description: String = "Definitive Anti-Dumping Duty"
    }
    case object A85 extends EUTaxCode {
      override def description: String = "Provisional Anti-Dumping Duty"
    }
    case object A90 extends EUTaxCode {
      override def description: String = "Definitive Countervailing Duty"
    }
    case object A95 extends EUTaxCode {
      override def description: String = "Provisional Countervailing Duty"
    }
    case object B05 extends EUTaxCode {
      override def description: String = "Value Added Tax"
    }

  }

  implicit val format: OFormat[TaxCode] = derived.oformat[TaxCode]()

}
