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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import org.scalamock.scalatest.MockFactory
import play.api.test.Helpers.{await, _}
import uk.gov.hmrc.cdsreimbursementclaim.connectors.HttpSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.test.BaseSpec
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global

class GetDeclarationConnectorSpec extends BaseSpec with MockFactory with HttpSupport {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mrn        = MRN.parse("21GBIDMSXBLNR06016").getOrElse(fail)
  val backEndUrl = s"http://localhost:7501/cds-reimbursement-claim/declaration/${mrn.value}"
  val connector  = new GetDeclarationConnector(mockHttp, appConfig)

  "DeclarationInfoConnector" when {

    "handling request for declaration" must {
      "do a post http call and get the ACC14 API response" in {
        val httpResponse = HttpResponse(200, "The Response")
        mockGet(backEndUrl)(Right(httpResponse))
        val declaration  = await(connector.getDeclarationInfo(mrn).value)
        declaration shouldBe Right(httpResponse)
      }
    }

    "return an error" when {
      "the call fails" in {
        val error       = new Exception("Socket connection error")
        mockGet(backEndUrl)(Left(error))
        val declaration = await(connector.getDeclarationInfo(mrn).value)
        declaration shouldBe Left(Error(error))
      }
    }

  }
}
