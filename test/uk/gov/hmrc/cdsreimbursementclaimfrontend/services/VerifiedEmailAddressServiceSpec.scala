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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.VerifiedEmailAddressConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.VerifiedEmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.VerifiedEmail
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

class VerifiedEmailAddressServiceSpec extends AnyWordSpec with Matchers with MockFactory with SessionSupport {

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  private val dataStoreConnector = mock[VerifiedEmailAddressConnector]

  private val dataStoreService: VerifiedEmailAddressService =
    new DefaultVerifiedEmailAddressService(dataStoreConnector, mockSessionCache)

  def mockDataStoreConnector(data: Eori)(response: Either[Error, HttpResponse]) =
    (dataStoreConnector
      .getVerifiedEmailAddress(_: Eori)(_: HeaderCarrier))
      .expects(data, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

  "VerifiedEmailAddressService" must {

    "use cached VerifiedEmail if available" in {
      val eori          = sample[Eori]
      val verifiedEmail = sample[VerifiedEmail]
      mockGetSession(SessionData(verifiedEmail = Some(verifiedEmail)))
      val response      = await(dataStoreService.getVerifiedEmailAddress(eori))
      response shouldBe Right(Some(verifiedEmail))
    }

    "retrieve and parse VerifiedEmail successfuly" in {
      val eori          = sample[Eori]
      val verifiedEmail = sample[VerifiedEmail]
      val httpResponse  = HttpResponse(200, Json.toJson(verifiedEmail).toString())
      mockGetSession(SessionData.empty)
      mockDataStoreConnector(eori)(Right(httpResponse))
      mockStoreSession(
        SessionData(verifiedEmail = Some(verifiedEmail))
      )(Right(()))
      val response      = await(dataStoreService.getVerifiedEmailAddress(eori))
      response shouldBe Right(Some(verifiedEmail))
    }

    "retrieve VerifiedEmail in incorrect format " in {
      val eori          = sample[Eori]
      val verifiedEmail = """{"address": "ups"}"""
      val httpResponse  = HttpResponse(200, verifiedEmail)
      mockGetSession(SessionData.empty)
      mockDataStoreConnector(eori)(Right(httpResponse))
      val response      = await(dataStoreService.getVerifiedEmailAddress(eori))
      response shouldBe Left(Error("""could not parse http response JSON: /timestamp: [error.path.missing]"""))
    }

    "sucessfull retrieve of empty email" in {
      val eori         = sample[Eori]
      val httpResponse = HttpResponse(404, "")
      mockGetSession(SessionData.empty)
      mockDataStoreConnector(eori)(Right(httpResponse))
      val response     = await(dataStoreService.getVerifiedEmailAddress(eori))
      response shouldBe Right(None)
    }

    "retrieve 500 response" in {
      val eori         = sample[Eori]
      val httpResponse = HttpResponse(500, "")
      mockGetSession(SessionData.empty)
      mockDataStoreConnector(eori)(Right(httpResponse))
      val response     = await(dataStoreService.getVerifiedEmailAddress(eori))
      response shouldBe Left(Error("Customs Data Store status: 500, body: "))
    }

  }
}
