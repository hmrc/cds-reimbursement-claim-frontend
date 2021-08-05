package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier

class AddressLookupConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

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
        |      bank-account-reputation {
        |        protocol = http
        |        host = localhost
        |        port = 9871
        |        business = /business/v2/assess
        |        personal = /personal/v3/assess
        |     }
        |
        |   }
        |}
        |""".stripMargin
    )
  )

}
