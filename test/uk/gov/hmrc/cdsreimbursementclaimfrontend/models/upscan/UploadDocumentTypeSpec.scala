/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal

class UploadDocumentTypeSpec extends AnyWordSpec with JsonFormatTest with Matchers {

  "UploadDocumentType" should {
    "serialize into a JSON format and back" in {
      validateJsonFormat[UploadDocumentType]("\"AirWayBill\"", UploadDocumentType.AirWayBill)
      validateJsonFormat[UploadDocumentType]("\"BillOfLading\"", UploadDocumentType.BillOfLading)
      validateJsonFormat[UploadDocumentType]("\"CommercialInvoice\"", UploadDocumentType.CommercialInvoice)
      validateJsonFormat[UploadDocumentType]("\"CorrespondenceTrader\"", UploadDocumentType.CorrespondenceTrader)
      validateJsonFormat[UploadDocumentType](
        "\"ImportAndExportDeclaration\"",
        UploadDocumentType.ImportAndExportDeclaration
      )
      validateJsonFormat[UploadDocumentType]("\"Other\"", UploadDocumentType.Other)
      validateJsonFormat[UploadDocumentType]("\"PackingList\"", UploadDocumentType.PackingList)
      validateJsonFormat[UploadDocumentType]("\"ProofOfAuthority\"", UploadDocumentType.ProofOfAuthority)
      validateJsonFormat[UploadDocumentType]("\"ScheduleOfMRNs\"", UploadDocumentType.ScheduleOfMRNs)
      validateJsonFormat[UploadDocumentType]("\"SubstituteEntry\"", UploadDocumentType.SubstituteEntry)

      UploadDocumentType.values.foreach(validateCanReadAndWriteJson[UploadDocumentType])
    }

    "have no duplicates in the securitiesDocumentTypes" in {
      for {
        rfs <- ReasonForSecurity.values
        mod <- TemporaryAdmissionMethodOfDisposal.values.map(Option.apply) + None
        poa <- Set(true, false)
      } UploadDocumentType
        .securitiesDocumentTypes(rfs, mod, poa)
        .foreach(dts => withClue(s"${dts.mkString(",")}")(dts.distinct.size.shouldBe(dts.size)))
    }

    "have no duplicates in the rejectedGoodsScheduledDocumentTypes" in {
      {
        val dts = UploadDocumentType.rejectedGoodsScheduledDocumentTypes
        dts.distinct.size === dts.size
      }
    }

    "have no duplicates in the rejectedGoodsMultipleDocumentTypes" in {
      {
        val dts = UploadDocumentType.rejectedGoodsMultipleDocumentTypes
        dts.distinct.size === dts.size
      }
    }

    "have no duplicates in the rejectedGoodsSingleDocumentTypes" in {
      {
        val dts = UploadDocumentType.rejectedGoodsSingleDocumentTypes
        dts.distinct.size === dts.size
      }
    }

    "have no duplicates in the c285DocumentTypes" in {
      {
        val dts = UploadDocumentType.c285DocumentTypes
        dts.distinct.size === dts.size
      }
    }
  }

}
