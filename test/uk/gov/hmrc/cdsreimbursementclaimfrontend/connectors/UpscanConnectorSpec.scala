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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.{Configuration, Environment}
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{FileUploadConfig, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UpscanInitiateRequest, UpscanUpload}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class UpscanConnectorSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  private val env           = Environment.simple()
  private val configuration = Configuration.load(env)

  val fileUploadConfig = new FileUploadConfig(configuration)
  val servicesConfig   = new ServicesConfig(configuration)
  val viewConfig       = new ViewConfig(configuration, servicesConfig)

  val connector = new DefaultUpscanConnector(mockHttp, fileUploadConfig, servicesConfig, viewConfig)

  "Upscan Connector" when {

    val reference = sample[UploadReference]
    val upload    = sample[UpscanUpload]
    val baseUrl   = "http://localhost:7501/cds-reimbursement-claim"

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "initiating an upscan transaction" must {
      val expectedUrl               = "http://localhost:9570/upscan/v2/initiate"
      val mockUpscanInitiateSuccess = Call("GET", "/mock-success")
      val mockUpscanInitiateFailure = Call("GET", "/mock-fail")
      val maxFileSize               = sample[Long]

      val payload = UpscanInitiateRequest(
        s"$baseUrl/upscan-call-back/upload-reference/${reference.value}",
        s"http://localhost:7500${mockUpscanInitiateSuccess.url}",
        s"http://localhost:7500${mockUpscanInitiateFailure.url}",
        0,
        maxFileSize
      )
      behave like connectorBehaviour(
        mockPost[UpscanInitiateRequest](
          expectedUrl,
          Seq.empty,
          payload
        ),
        () => connector.initiate(mockUpscanInitiateFailure, mockUpscanInitiateSuccess, reference, maxFileSize)
      )
    }

    "getting the upscan upload" must {
      val expectedUrl = s"$baseUrl/upscan/upload-reference/${reference.value}"
      behave like connectorBehaviour(
        mockGet(expectedUrl)(_),
        () => connector.getUpscanUpload(reference)
      )
    }

    "saving upscan upload" when {
      behave like connectorBehaviour(
        mockPost(s"$baseUrl/upscan", Seq.empty, upload)(_),
        () => connector.saveUpscanUpload(upload)
      )
    }

  }

}
