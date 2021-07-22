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

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload.FileUploadServices
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UpscanInitiateRequest, UpscanUpload}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class UpscanConnectorSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val configuration: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      upscan-initiate {
        |        protocol = http
        |        host     = host2
        |        port     = 123
        |        supporting-evidence {
        |         max-uploads   = 10
        |         max-file-size = 1234
        |        }
        |      },
        |      cds-reimbursement-claim {
        |        protocol = http
        |        host     = host3
        |        port     = 123
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val connector = new DefaultUpscanConnector(mockHttp, configuration, new ServicesConfig(configuration))

  "Upscan Connector" when {

    val reference         = sample[UploadReference]
    val upload            = sample[UpscanUpload]
    val baseUrl           = "http://host3:123/cds-reimbursement-claim"
    val fileUploadService = new FileUploadServices(configuration)
    import fileUploadService._

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "initiating an upscan transaction" must {
      val expectedUrl               = "http://host2:123/upscan/v2/initiate"
      val mockUpscanInitiateSuccess = Call("GET", "/mock-success")
      val mockUpscanInitiateFailure = Call("GET", "/mock-fail")

      val payload = UpscanInitiateRequest(
        s"$baseUrl/upscan-call-back/upload-reference/${reference.value}",
        s"host1.com${mockUpscanInitiateSuccess.url}",
        s"host1.com${mockUpscanInitiateFailure.url}",
        0,
        1234
      )
      behave like connectorBehaviour(
        mockPost[UpscanInitiateRequest](
          expectedUrl,
          Seq.empty,
          payload
        ),
        () =>
          connector.initiate[SupportingEvidencesAnswer](mockUpscanInitiateFailure, mockUpscanInitiateSuccess, reference)
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
