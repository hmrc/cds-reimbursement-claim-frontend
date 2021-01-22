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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers.{await, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationInfoConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarationInfoRequest, DeclarationInfoResponse, Error}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DeclarationInfoServiceSpec extends AnyWordSpec with Matchers with MockFactory {

  val declarationInfoConnector = mock[DeclarationInfoConnector]

  val declarationInfoService = new DeclarationInfoService(declarationInfoConnector)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val declarationId          = "GB349970632046"
  val declarationRequest     = DeclarationInfoRequest(declarationId)
  val declarationRequestJson = Json.toJson(declarationRequest)

  implicit val request: Request[_] = FakeRequest()
  val emptyResponse: JsValue       = Json.parse(s"""{
                                                       |  "overpaymentDeclarationDisplayResponse": {
                                                       |    "responseCommon": {
                                                       |      "status": "OK",
                                                       |      "processingDate": "2020-42-84T35:41:48Z"
                                                       |    }
                                                       |  }
                                                       |}""".stripMargin)

  val bigResponse: JsValue = Json.parse(s"""{
                                          |   "overpaymentDeclarationDisplayResponse":{
                                          |      "responseCommon":{
                                          |         "status":"OK",
                                          |         "processingDate":"2001-12-17T09:30:47Z"
                                          |      },
                                          |      "responseDetail":{
                                          |         "declarationId":"$declarationId",
                                          |         "acceptanceDate":"2019-08-13",
                                          |         "declarantReferenceNumber":"XFGLKJDSE5GDPOIJEW985T",
                                          |         "securityReason":"IPR",
                                          |         "btaDueDate":"2019-09-13",
                                          |         "procedureCode":"71",
                                          |         "btaSource":"DMS",
                                          |         "declarantDetails":{
                                          |            "declarantEORI":"GB3745678934000",
                                          |            "legalName":"Fred Bloggs and Co Ltd",
                                          |            "establishmentAddress":{
                                          |               "addressLine1":"10 Rillington Place",
                                          |               "addressLine2":"London",
                                          |               "addressLine3":"Pimlico",
                                          |               "postalCode":"W11 1RH",
                                          |               "countryCode":"GB"
                                          |            },
                                          |            "contactDetails":{
                                          |               "contactName":"Angela Smith",
                                          |               "addressLine1":"J P Jones Insolvency Ltd",
                                          |               "addressLine2":"14 Briar Lane",
                                          |               "addressLine3":"Pimlico",
                                          |               "postalCode":"W11 1QT",
                                          |               "countryCode":"GB",
                                          |               "telephone":"0270 112 3476",
                                          |               "emailAddress":"fred@bloggs.com"
                                          |            }
                                          |         },
                                          |         "consigneeDetails":{
                                          |            "consigneeEORI":"GB562485153000",
                                          |            "legalName":"Swift Goods Ltd",
                                          |            "establishmentAddress":{
                                          |               "addressLine1":"14 Briar Lane",
                                          |               "addressLine2":"London",
                                          |               "addressLine3":"Pimlico",
                                          |               "countryCode":"GB"
                                          |            },
                                          |            "contactDetails":{
                                          |               "contactName":"Frank Sidebotham",
                                          |               "addressLine1":"J P Jones Insolvency Ltd",
                                          |               "addressLine2":"14 Briar Lane",
                                          |               "addressLine3":"Pimlico",
                                          |               "postalCode":"W11 1QT",
                                          |               "countryCode":"GB",
                                          |               "telephone":"0207 678 3243",
                                          |               "emailAddress":"enquiries@swftgoods.com"
                                          |            }
                                          |         },
                                          |         "accountDetails":[
                                          |            {
                                          |               "accountType":"001",
                                          |               "accountNumber":"8901112",
                                          |               "eori":"8432569",
                                          |               "legalName":"Fred Bloggs and Co Ltd",
                                          |               "contactDetails":{
                                          |                  "contactName":"Angela Smith",
                                          |                  "addressLine1":"J P Jones Insolvency Ltd",
                                          |                  "addressLine2":"14 Briar Lane",
                                          |                  "addressLine3":"Holborn",
                                          |                  "addressLine4":"London",
                                          |                  "countryCode":"GB",
                                          |                  "telephone":"0270 112 3476",
                                          |                  "emailAddress":"fred@bloggs.com"
                                          |               }
                                          |            },
                                          |            {
                                          |               "accountType":"002",
                                          |               "accountNumber":"8901113",
                                          |               "eori":"8432563",
                                          |               "legalName":"Fred Bloggs and Co Ltd",
                                          |               "contactDetails":{
                                          |                  "contactName":"Angela Smith",
                                          |                  "addressLine1":"J P Jones Insolvency Ltd",
                                          |                  "addressLine2":"14 Briar Lane",
                                          |                  "addressLine3":"London",
                                          |                  "countryCode":"GB",
                                          |                  "telephone":"0270 112 3476",
                                          |                  "emailAddress":"fred@bloggs.com"
                                          |               }
                                          |            }
                                          |         ],
                                          |         "bankDetails":{
                                          |            "consigneeBankDetails":{
                                          |               "accountHolderName":"Swift Goods Ltd",
                                          |               "sortCode":"125841",
                                          |               "accountNumber":"01478523"
                                          |            },
                                          |            "declarantBankDetails":{
                                          |               "accountHolderName":"Fred Bloggs and Co Ltd",
                                          |               "sortCode":"653214",
                                          |               "accountNumber":"54789632"
                                          |            }
                                          |         },
                                          |         "securityDetails":[
                                          |            {
                                          |               "securityDepositId":"ABC0123456",
                                          |               "totalAmount":"14585.52",
                                          |               "amountPaid":"14585.52",
                                          |               "paymentMethod":"001",
                                          |               "paymentReference":"SGL SECURITY 001",
                                          |               "taxDetails":[
                                          |                  {
                                          |                     "taxType":"A00",
                                          |                     "amount":"6000.00"
                                          |                  },
                                          |                  {
                                          |                     "taxType":"584",
                                          |                     "amount":"8085.52"
                                          |                  }
                                          |               ]
                                          |            },
                                          |            {
                                          |               "securityDepositId":"DEF6543210",
                                          |               "totalAmount":"500.00",
                                          |               "amountPaid":"300.00",
                                          |               "paymentMethod":"002",
                                          |               "paymentReference":"SGL SECURITY 002",
                                          |               "taxDetails":[
                                          |                  {
                                          |                     "taxType":"B00",
                                          |                     "amount":"100.00"
                                          |                  },
                                          |                  {
                                          |                     "taxType":"478",
                                          |                     "amount":"200.00"
                                          |                  }
                                          |               ]
                                          |            }
                                          |         ]
                                          |      }
                                          |   }
                                          |}""".stripMargin)

  def errorResponse(errorMessage: String): JsValue = Json.parse(s"""{
                                                                   |   "ErrorDetails":{
                                                                   |      "ProcessingDateTime":"2016-10-10T13:52:16Z",
                                                                   |      "CorrelationId":"d60de98c-f499-47f5-b2d6-e80966e8d19e",
                                                                   |      "ErrorMessage":"$errorMessage"
                                                                   |    }
                                                                   |}""".stripMargin)

  def mockDeclarationConnector(response: Either[Error, HttpResponse]) =
    (declarationInfoConnector
      .getDeclarationInfo(_: JsValue)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

  "Declaration Information Request Service" when {
    "handling a request returns" should {
      "handle successful submits" when {
        "there is a valid empty payload" in {
          mockDeclarationConnector(Right(HttpResponse(200, emptyResponse, Map.empty[String, Seq[String]])))
          await(declarationInfoService.getDeclarationInfo(declarationId).value) shouldBe Right(
            emptyResponse.as[DeclarationInfoResponse]
          )
        }

        "there is a valid big payload" in {
          mockDeclarationConnector(Right(HttpResponse(200, bigResponse, Map.empty[String, Seq[String]])))
          await(declarationInfoService.getDeclarationInfo(declarationId).value) shouldBe Right(
            bigResponse.as[DeclarationInfoResponse]
          )
        }
      }

      "handle unsuccesful submits" when {
        "400 response" in {
          val decInfoResponse = errorResponse("Invalid Request")
          mockDeclarationConnector(Right(HttpResponse(400, decInfoResponse, Map.empty[String, Seq[String]])))
          val response        = await(declarationInfoService.getDeclarationInfo(declarationId).value)
          response.fold(_.message should include("Invalid Request"), _ => fail())
        }

        "500 response" in {
          val decInfoResponse = errorResponse("invalid JSON format")
          mockDeclarationConnector(Right(HttpResponse(500, decInfoResponse, Map.empty[String, Seq[String]])))
          val response        = await(declarationInfoService.getDeclarationInfo(declarationId).value)
          response.fold(_.message should include("invalid JSON format"), _ => fail())
        }

        "Invalid Json response" in {
          mockDeclarationConnector(Right(HttpResponse(200, """{"a"-"b"}""", Map.empty[String, Seq[String]])))
          val response = await(declarationInfoService.getDeclarationInfo(declarationId).value)
          response.fold(_.message should include("Unexpected character"), _ => fail())
        }

      }
    }
  }

}
