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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.CDSReimbursementClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.C285ClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen.genBankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmitClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ClaimServiceSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  val mockClaimConnector: ClaimConnector                                 = mock[ClaimConnector]
  val mockCDSReimbursementClaimConnector: CDSReimbursementClaimConnector = mock[CDSReimbursementClaimConnector]

  val language: Lang = Lang("en")

  val claimService = new DefaultClaimService(mockClaimConnector, mockCDSReimbursementClaimConnector)

  val okSubmitClaimResponse: JsValue = Json.parse(
    """
      | {
      |   "caseNumber" : "NDRC-1"
      | }
      |""".stripMargin
  )

  val okDisplayDeclaration: JsValue = Json.parse(
    """
      |{
      |    "displayResponseDetail": {
      |        "declarationId": "d-1",
      |        "acceptanceDate": "2020-10-20",
      |        "procedureCode": "p-1",
      |        "declarantDetails": {
      |            "declarantEORI": "F-1",
      |            "legalName": "Fred Bread",
      |            "establishmentAddress": {
      |                "addressLine1": "line-1",
      |                "countryCode": "GB"
      |            }
      |        }
      |    }
      |}
      |""".stripMargin
  )

  val okResponse: JsValue = Json.parse(
    """
      |{
      |    "PostNewClaimsResponse": {
      |        "ResponseCommon": {
      |            "Status": "OK",
      |            "ProcessingDateTime": "2020-12-23T16:58:28Z",
      |            "CDFPayCaseNumber": "NDRC-1234",
      |            "CDFPayService": "NDRC"
      |        }
      |    }
      |}
      |""".stripMargin
  )

  def errorResponse(errorMessage: String): JsValue = Json.parse(
    s"""
      |{
      |    "ErrorDetails": {
      |        "ProcessingDateTime": "2016-10-10T13:52:16Z",
      |        "CorrelationId": "d60de98c-f499-47f5-b2d6-e80966e8d19e",
      |        "ErrorMessage": "$errorMessage"
      |    }
      |}
      |""".stripMargin
  )

  def mockSubmitClaim(c285Claim: C285ClaimRequest)(
    response: Either[Error, HttpResponse]
  ): CallHandler2[C285ClaimRequest, HeaderCarrier, EitherT[Future, Error, HttpResponse]] =
    (mockClaimConnector
      .submitClaim(_: C285ClaimRequest)(_: HeaderCarrier))
      .expects(c285Claim, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetDisplayDeclaration(mrn: MRN)(
    response: Either[Error, HttpResponse]
  ): CallHandler2[MRN, HeaderCarrier, EitherT[Future, Error, HttpResponse]] =
    (mockCDSReimbursementClaimConnector
      .getDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(mrn, *)
      .returning(EitherT.fromEither[Future](response))

  "Claim Service" when {

    "handling request to submit a claim" must {

      "return an error" when {

        "the http call fails" in {
          val submitClaimRequest = sample[C285ClaimRequest]
          mockSubmitClaim(submitClaimRequest)(Left(Error("boom!")))
          await(claimService.submitClaim(submitClaimRequest, language).value).isLeft shouldBe true
        }

        "the http call comes back with invalid json" in {
          val submitClaimRequest = sample[C285ClaimRequest]
          mockSubmitClaim(submitClaimRequest)(Right(HttpResponse(INTERNAL_SERVER_ERROR, "---")))
          await(claimService.submitClaim(submitClaimRequest, language).value).isLeft shouldBe true
        }

        "the http call comes back with a status other than 200" in {
          val submitClaimRequest = sample[C285ClaimRequest]
          mockSubmitClaim(submitClaimRequest)(Right(HttpResponse(INTERNAL_SERVER_ERROR, "{}")))
          await(claimService.submitClaim(submitClaimRequest, language).value).isLeft shouldBe true
        }
      }

      "return an ok response" when {
        "the http response came back with a 200 OK" in {
          val submitClaimRequest = sample[C285ClaimRequest]
          mockSubmitClaim(submitClaimRequest)(
            Right(HttpResponse(OK, okSubmitClaimResponse, Map[String, Seq[String]]()))
          )
          await(claimService.submitClaim(submitClaimRequest, language).value) shouldBe Right(
            SubmitClaimResponse("NDRC-1")
          )
        }

      }

    }

    "handling request to get a declaration" must {

      val mrn = sample[MRN]

      "return an error" when {

        "the http call fails" in {
          mockGetDisplayDeclaration(mrn)(Left(Error("boom!")))
          await(claimService.getDisplayDeclaration(mrn).value).isLeft shouldBe true
        }

        "the http call comes back with invalid json" in {
          mockGetDisplayDeclaration(mrn)(Right(HttpResponse(INTERNAL_SERVER_ERROR, "---")))
          await(claimService.getDisplayDeclaration(mrn).value).isLeft shouldBe true
        }

        "the http call comes back with a status other than 200" in {
          mockGetDisplayDeclaration(mrn)(Right(HttpResponse(INTERNAL_SERVER_ERROR, "{}")))
          await(claimService.getDisplayDeclaration(mrn).value).isLeft shouldBe true
        }
      }

      "return a successful response" when {

        val displayDeclaration = DisplayDeclaration(
          displayResponseDetail = DisplayResponseDetail(
            declarantReferenceNumber = None,
            securityReason = None,
            btaDueDate = None,
            btaSource = None,
            declarationId = "d-1",
            acceptanceDate = "2020-10-20",
            procedureCode = "p-1",
            consigneeDetails = None,
            accountDetails = None,
            bankDetails = None,
            maskedBankDetails = None,
            ndrcDetails = None,
            declarantDetails = DeclarantDetails(
              declarantEORI = "F-1",
              legalName = "Fred Bread",
              establishmentAddress = EstablishmentAddress(
                addressLine1 = "line-1",
                addressLine2 = None,
                addressLine3 = None,
                postalCode = None,
                countryCode = "GB"
              ),
              contactDetails = None
            )
          )
        )

        "the http response came back with a 200 OK" in {
          mockGetDisplayDeclaration(mrn)(Right(HttpResponse(OK, okDisplayDeclaration, Map[String, Seq[String]]())))
          await(claimService.getDisplayDeclaration(mrn).value) shouldBe Right(Some(displayDeclaration))
        }

        "the http response came back with a 204 NO CONTENT" in {
          mockGetDisplayDeclaration(mrn)(
            Right(HttpResponse(NO_CONTENT, okSubmitClaimResponse, Map[String, Seq[String]]()))
          )
          await(claimService.getDisplayDeclaration(mrn).value) shouldBe Right(None)
        }

      }

    }

    "handling requests to verify a business account" should {
      def businessRequest(bankAccountDetails: BankAccountDetails) = Json.obj(
        "account" -> Json.obj(
          "sortCode"      -> bankAccountDetails.sortCode.value,
          "accountNumber" -> bankAccountDetails.accountNumber.value
        )
      )

      def mockBusinessReputationConnector(data: JsValue)(response: Either[Error, HttpResponse]) =
        (mockCDSReimbursementClaimConnector
          .getBusinessReputation(_: JsValue)(_: HeaderCarrier))
          .expects(data, *)
          .returning(EitherT.fromEither[Future](response))
          .atLeastOnce()

      "retrieve and parse data" in forAll(
        genBankAccountDetails,
        arbitraryBusinessCompleteResponse.arbitrary
      ) { (bankAccount, businessResponse) =>
        val httpResponse = HttpResponse(200, Json.toJson(businessResponse).toString())
        mockBusinessReputationConnector(businessRequest(bankAccount))(Right(httpResponse))
        val response     = await(claimService.getBusinessAccountReputation(bankAccount).value)
        response shouldBe Right(businessResponse.toCommonResponse())
      }

      "parse error response" in forAll(
        genBankAccountDetails,
        arbitraryReputationErrorResponse.arbitrary
      ) { (bankAccount, errorResponse) =>
        val httpResponse = HttpResponse(400, Json.toJson(errorResponse).toString())
        mockBusinessReputationConnector(businessRequest(bankAccount))(Right(httpResponse))
        val response     = await(claimService.getBusinessAccountReputation(bankAccount).value)
        response shouldBe Right(errorResponse.toCommonResponse())
      }

      "Fail when the connector fails" in forAll(
        genBankAccountDetails
      ) { bankAccount =>
        val httpResponse = HttpResponse(500, "")
        mockBusinessReputationConnector(businessRequest(bankAccount))(Right(httpResponse))
        val response     = await(claimService.getBusinessAccountReputation(bankAccount).value)
        response shouldBe Left(Error("Call to Business Reputation Service (BARS) failed with: 500"))
      }

      "Fail when the returned JSON cannot be parsed" in forAll(
        genBankAccountDetails
      ) { bankAccount =>
        val httpResponse = HttpResponse(200, """{"BARS" : "maybe not"}""")
        mockBusinessReputationConnector(businessRequest(bankAccount))(Right(httpResponse))
        val response     = await(claimService.getBusinessAccountReputation(bankAccount).value)
        response.isLeft                                                                      shouldBe true
        response.left.getOrElse(fail).message.contains("could not parse http response JSON") shouldBe true
      }
    }

    "handling requests to verify a personal account" should {
      def personalRequest(bankAccountDetails: BankAccountDetails, postCode: String) = Json.obj(
        "account" -> Json.obj(
          "sortCode"      -> bankAccountDetails.sortCode.value,
          "accountNumber" -> bankAccountDetails.accountNumber.value
        ),
        "subject" -> Json.obj(
          "name"    -> bankAccountDetails.accountName.value,
          "address" -> Json.obj(
            "lines"    -> Json.arr(" "),
            "postcode" -> postCode
          )
        )
      )

      def mockPersonalReputationConnector(data: JsValue)(response: Either[Error, HttpResponse]) =
        (mockCDSReimbursementClaimConnector
          .getPersonalReputation(_: JsValue)(_: HeaderCarrier))
          .expects(data, *)
          .returning(EitherT.fromEither[Future](response))
          .atLeastOnce()

      "retrieve and parse data" in forAll(
        genBankAccountDetails,
        genPostcode,
        arbitraryPersonalCompleteResponse.arbitrary
      ) { (bankAccount, postCode, personalResponse) =>
        val httpResponse = HttpResponse(200, Json.toJson(personalResponse).toString())
        mockPersonalReputationConnector(personalRequest(bankAccount, postCode))(Right(httpResponse))
        val response     = await(claimService.getPersonalAccountReputation(bankAccount, Some(postCode)).value)
        response shouldBe Right(personalResponse.toCommonResponse())
      }

      "parse error response" in forAll(
        genBankAccountDetails,
        genPostcode,
        arbitraryReputationErrorResponse.arbitrary
      ) { (bankAcount, postCode, errorResponse) =>
        val httpResponse = HttpResponse(400, Json.toJson(errorResponse).toString())
        mockPersonalReputationConnector(personalRequest(bankAcount, postCode))(Right(httpResponse))
        val response     = await(claimService.getPersonalAccountReputation(bankAcount, Some(postCode)).value)
        response shouldBe Right(errorResponse.toCommonResponse())
      }

      "Fail when the connector fails" in forAll(genBankAccountDetails, genPostcode) { (bankAcount, postCode) =>
        val httpResponse = HttpResponse(500, "")
        mockPersonalReputationConnector(personalRequest(bankAcount, postCode))(Right(httpResponse))
        val response     = await(claimService.getPersonalAccountReputation(bankAcount, Some(postCode)).value)
        response shouldBe Left(Error("Call to Business Reputation Service (BARS) failed with: 500"))
      }

      "Fail when the returned JSON cannot be parsed" in forAll(genBankAccountDetails, genPostcode) {
        (bankAcount, postCode) =>
          val httpResponse = HttpResponse(200, """{"BARS" : "maybe not"}""")
          mockPersonalReputationConnector(personalRequest(bankAcount, postCode))(Right(httpResponse))
          val response     = await(claimService.getPersonalAccountReputation(bankAcount, Some(postCode)).value)
          response.isLeft                                                                      shouldBe true
          response.left.getOrElse(fail).message.contains("could not parse http response JSON") shouldBe true
      }

    }
  }

}
