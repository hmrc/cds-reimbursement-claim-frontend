package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import com.google.inject.Inject
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton

@Singleton
class AddressLookupConfig @Inject() (config: ServicesConfig) {

  private val serviceName = "address-lookup-frontend"

  lazy val serviceUrl: String = config.baseUrl(serviceName)

  lazy val triggerAddressLookupUrl: String = createUrlReadingProperty("init-endpoint")

  lazy val retrieveAddressUrl: String = createUrlReadingProperty("address-retrieve-endpoint")

  private def createUrlReadingProperty(pathConfigKey: String): String =
    s"$serviceUrl${config.getString(s"microservice.services.$serviceName.$pathConfigKey")}"
}
