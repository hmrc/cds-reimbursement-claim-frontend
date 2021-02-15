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
import org.scalatest.Ignore
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.{CDSReimbursementClaimConnector, ClaimConnector}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Ignore
class GetDeclarationServiceSpec extends AnyWordSpec with Matchers with MockFactory {

  val backendConnector = mock[CDSReimbursementClaimConnector]
  val claimConnector   = mock[ClaimConnector]

  val getDeclarationService = new DefaultClaimService(claimConnector, backendConnector)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val declarationId = "21GBIDMSXBLNR06016"
  val mrn           = MRN(declarationId)

  implicit val request: Request[_] = FakeRequest()
  val emptyResponse: JsValue       = Json.parse(s"""{
                                                       |  "overpaymentDeclarationDisplayResponse": {
                                                       |    "responseCommon": {
                                                       |      "status": "OK",
                                                       |      "processingDate": "2020-42-84T35:41:48Z"
                                                       |    }
                                                       |  }
                                                       |}""".stripMargin)

  val bigResponse: JsValue                         = Json.parse(
    s"""
       |{
       |    "declarantId": "$declarationId",
       |    "acceptanceDate": "RJBTQORGBZ",
       |    "declarantDetails": {
       |        "declarantEORI": "AXWWH",
       |        "legalName": "KGN",
       |        "establishmentAddress": {
       |            "addressLine1": "J",
       |            "addressLine3": "N",
       |            "postalCode": "U",
       |            "countryCode": "D",
       |            "addressLine2": "Q"
       |        },
       |        "contactDetails": {
       |            "addressLine1": "T",
       |            "addressLine4": "G",
       |            "emailAddress": "J",
       |            "addressLine3": "Z",
       |            "postalCode": "W",
       |            "countryCode": "I",
       |            "addressLine2": "N",
       |            "telephone": "B"
       |        }
       |    },
       |    "consigneeDetails": {
       |        "consigneeEORI": "DOB",
       |        "legalName": "NOAH",
       |        "establishmentAddress": {
       |            "addressLine1": "O",
       |            "addressLine3": "K",
       |            "postalCode": "W",
       |            "countryCode": "I",
       |            "addressLine2": "C"
       |        },
       |        "contactDetails": {
       |            "addressLine1": "L",
       |            "addressLine4": "C",
       |            "emailAddress": "N",
       |            "addressLine3": "W",
       |            "postalCode": "O",
       |            "contactName": "R",
       |            "countryCode": "K",
       |            "addressLine2": "B",
       |            "telephone": "T"
       |        }
       |    },
       |    "maskedBankDetails": {
       |        "consigneeBankDetails": {
       |            "accountHolderName": "XSW",
       |            "sortCode": "ZN",
       |            "accountNumber": "LO"
       |        }
       |    },
       |    "securityDetails": [
       |        {
       |            "securityDepositId": "U",
       |            "totalAmount": "R",
       |            "amountPaid": "UR",
       |            "paymentMethod": "I",
       |            "paymentReference": "T",
       |            "taxDetails": [
       |                {
       |                    "taxType": "V",
       |                    "amount": "K"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "HIV",
       |            "totalAmount": "XM",
       |            "amountPaid": "W",
       |            "paymentMethod": "LR",
       |            "paymentReference": "PW",
       |            "taxDetails": [
       |                {
       |                    "taxType": "Q",
       |                    "amount": "N"
       |                },
       |                {
       |                    "taxType": "X",
       |                    "amount": "E"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "H",
       |            "totalAmount": "RA",
       |            "amountPaid": "VS",
       |            "paymentMethod": "SPO",
       |            "paymentReference": "SV",
       |            "taxDetails": [
       |                {
       |                    "taxType": "T",
       |                    "amount": "W"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "VT",
       |            "totalAmount": "OV",
       |            "amountPaid": "CVG",
       |            "paymentMethod": "AN",
       |            "paymentReference": "FH",
       |            "taxDetails": [
       |                {
       |                    "taxType": "E",
       |                    "amount": "W"
       |                },
       |                {
       |                    "taxType": "U",
       |                    "amount": "Z"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "W",
       |            "totalAmount": "MTB",
       |            "amountPaid": "FKL",
       |            "paymentMethod": "B",
       |            "paymentReference": "T",
       |            "taxDetails": [
       |                {
       |                    "taxType": "C",
       |                    "amount": "M"
       |                },
       |                {
       |                    "taxType": "Y",
       |                    "amount": "D"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "Y",
       |            "totalAmount": "SM",
       |            "amountPaid": "L",
       |            "paymentMethod": "OX",
       |            "paymentReference": "X",
       |            "taxDetails": []
       |        },
       |        {
       |            "securityDepositId": "F",
       |            "totalAmount": "Q",
       |            "amountPaid": "M",
       |            "paymentMethod": "T",
       |            "paymentReference": "O",
       |            "taxDetails": [
       |                {
       |                    "taxType": "P",
       |                    "amount": "L"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "AIQ",
       |            "totalAmount": "ZRD",
       |            "amountPaid": "GZ",
       |            "paymentMethod": "J",
       |            "paymentReference": "S",
       |            "taxDetails": []
       |        },
       |        {
       |            "securityDepositId": "WD",
       |            "totalAmount": "R",
       |            "amountPaid": "MIF",
       |            "paymentMethod": "ENM",
       |            "paymentReference": "OWC",
       |            "taxDetails": [
       |                {
       |                    "taxType": "O",
       |                    "amount": "Q"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "BOY",
       |            "totalAmount": "P",
       |            "amountPaid": "BP",
       |            "paymentMethod": "XMB",
       |            "paymentReference": "OA",
       |            "taxDetails": [
       |                {
       |                    "taxType": "O",
       |                    "amount": "S"
       |                },
       |                {
       |                    "taxType": "E",
       |                    "amount": "H"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "MM",
       |            "totalAmount": "AX",
       |            "amountPaid": "XJ",
       |            "paymentMethod": "Y",
       |            "paymentReference": "B",
       |            "taxDetails": [
       |                {
       |                    "taxType": "F",
       |                    "amount": "J"
       |                },
       |                {
       |                    "taxType": "K",
       |                    "amount": "F"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "JV",
       |            "totalAmount": "PZH",
       |            "amountPaid": "CSK",
       |            "paymentMethod": "AKO",
       |            "paymentReference": "HM",
       |            "taxDetails": [
       |                {
       |                    "taxType": "D",
       |                    "amount": "L"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "HA",
       |            "totalAmount": "Y",
       |            "amountPaid": "EE",
       |            "paymentMethod": "MTX",
       |            "paymentReference": "MG",
       |            "taxDetails": [
       |                {
       |                    "taxType": "P",
       |                    "amount": "E"
       |                },
       |                {
       |                    "taxType": "O",
       |                    "amount": "N"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "MVU",
       |            "totalAmount": "JFG",
       |            "amountPaid": "HPP",
       |            "paymentMethod": "NFK",
       |            "paymentReference": "C",
       |            "taxDetails": [
       |                {
       |                    "taxType": "R",
       |                    "amount": "Q"
       |                },
       |                {
       |                    "taxType": "X",
       |                    "amount": "U"
       |                }
       |            ]
       |        },
       |        {
       |            "securityDepositId": "AO",
       |            "totalAmount": "Q",
       |            "amountPaid": "U",
       |            "paymentMethod": "Y",
       |            "paymentReference": "FDM",
       |            "taxDetails": [
       |                {
       |                    "taxType": "H",
       |                    "amount": "O"
       |                },
       |                {
       |                    "taxType": "G",
       |                    "amount": "H"
       |                }
       |            ]
       |        }
       |    ]
       |}
       |""".stripMargin
  )
  def errorResponse(errorMessage: String): JsValue = Json.parse(s"""{
                                                                   |   "ErrorDetails":{
                                                                   |      "ProcessingDateTime":"2016-10-10T13:52:16Z",
                                                                   |      "CorrelationId":"d60de98c-f499-47f5-b2d6-e80966e8d19e",
                                                                   |      "ErrorMessage":"$errorMessage"
                                                                   |    }
                                                                   |}""".stripMargin)

  def mockDeclarationConnector(mrn: MRN)(response: Either[Error, HttpResponse]) =
    (backendConnector
      .getDeclarationDetails(_: MRN)(_: HeaderCarrier))
      .expects(mrn, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

//  "Declaration Information Request Service" when {
//    "handling a request returns" should {
//      "handle successful submits" when {
//        "there is a valid empty payload" in {
//          mockDeclarationConnector(mrn)(Right(HttpResponse(200, emptyResponse, Map.empty[String, Seq[String]])))
//          await(getDeclarationService.getDeclaration(mrn).value).isLeft shouldBe true
//        }
//
//        "there is a valid big payload" in {
//          mockDeclarationConnector(mrn)(Right(HttpResponse(200, bigResponse, Map.empty[String, Seq[String]])))
//          await(getDeclarationService.getDeclaration(mrn).value).isRight shouldBe true
//        }
//      }
//
//      "handle unsuccessful submits" when {
//        "400 response" in {
//          val decInfoResponse = errorResponse("call to get declaration details ")
//          mockDeclarationConnector(mrn)(Right(HttpResponse(400, decInfoResponse, Map.empty[String, Seq[String]])))
//          val response        = await(getDeclarationService.getDeclaration(mrn).value)
//          response.fold(_.message should include("call to get declaration details "), _ => fail())
//        }
//
//        "500 response" in {
//          val decInfoResponse = errorResponse("call to get declaration details ")
//          mockDeclarationConnector(mrn)(Right(HttpResponse(500, decInfoResponse, Map.empty[String, Seq[String]])))
//          val response        = await(getDeclarationService.getDeclaration(mrn).value)
//          response.fold(_.message should include("call to get declaration details "), _ => fail())
//        }
//
//        "Invalid Json response" in {
//          mockDeclarationConnector(mrn)(Right(HttpResponse(200, """{"a"-"b"}""", Map.empty[String, Seq[String]])))
//          val response = await(getDeclarationService.getDeclaration(mrn).value)
//          response.fold(_.message should include("Unexpected character"), _ => fail())
//        }
//
//      }
//    }
//  }

}
