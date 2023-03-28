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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Configuration
import play.api.test.Helpers.await
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ExistingClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReasonForSecurityGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExistingClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global

class CDSReimbursementClaimConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec
    with ScalaCheckPropertyChecks {

  val config: Configuration      = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      cds-reimbursement-claim {
        |        protocol = http
        |        host     = localhost
        |        port     = 7501
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )
  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val timeout: Timeout  = Timeout(5, TimeUnit.SECONDS)

  val connector = new DefaultCDSReimbursementClaimConnector(mockHttp, new ServicesConfig(config))

  "CDS Reimbursement Connector" when {

    "handling requests to get a declaration" must {
      val mrn = sample[MRN]
      val url = s"http://localhost:7501/cds-reimbursement-claim/declaration/${mrn.value}"

      behave like connectorBehaviour(
        mockGet(url)(_),
        () => connector.getDeclaration(mrn)
      )
    }

  }

  "CDS Reimbursement Connector Duplicate" should {
    def url(mrn: MRN, reasonForSecurity: ReasonForSecurity) =
      s"http://localhost:7501/cds-reimbursement-claim/declaration/${mrn.value}/claim-exists?reasonForSecurity=$reasonForSecurity"

    "handling requests to see if a declaration is a duplicate do a http call and return the result" in forAll {
      (mrn: MRN, reason: ReasonForSecurity, existingClaim: ExistingClaim) =>
        mockGet(url(mrn, reason))(Some(existingClaim))
        await(connector.getIsDuplicate(mrn, reason).value) shouldBe Right(existingClaim)
    }

    "return an error when the call fails" in forAll { (mrn: MRN, reason: ReasonForSecurity) =>
      mockGet(url(mrn, reason))(None)
      await(connector.getIsDuplicate(mrn, reason).value).isLeft shouldBe true
    }
  }
}
