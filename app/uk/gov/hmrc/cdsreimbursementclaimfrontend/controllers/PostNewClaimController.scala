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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.implicits._
import javax.inject.Inject
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Results}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AppConfig
import uk.gov.hmrc.http.HttpErrorFunctions.is2xx
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class PostNewClaimController @Inject() (implicit
  mcc: MessagesControllerComponents,
  appConfig: AppConfig,
  httpClient: HttpClient,
  ec: ExecutionContext
) extends FrontendController(mcc)
    with Logging {

  type ErrorMessage = String

  val claim: Action[AnyContent] = Action.async { implicit request =>
    (request.method match {
      case "GET"  => testRequestBody.asRight[ErrorMessage]
      case "POST" => Either.fromOption[ErrorMessage, JsValue](request.body.asJson, "Request Body is not Json!")
    }).map { body =>
      httpClient
        .POST[JsValue, HttpResponse](appConfig.claimsEndpoint, body)
        .map { response =>
          if (!is2xx(response.status))
            logger.warn(s"Downstream error,response status: ${response.status}, body: ${response.body}")
          Results.Status(response.status)(response.body)
        }
    }.leftMap(error => Future.successful(BadRequest(error)))
      .merge
  }

  val testRequestBody: JsValue = Json.parse("""{
                                     |  "PostNewClaims": {
                                     |    "RequestCommon": {
                                     |      "OriginatingSystem": "CDSF",
                                     |      "ReceiptDate": "2010-06-19T11:52:12Z",
                                     |      "AcknowledgementReference": "ACK0701HPRER-21"
                                     |    },
                                     |    "RequestDetail": {
                                     |      "CDFPayService": "NDRC",
                                     |      "ClaimType": "C285",
                                     |      "CaseType": "Bulk",
                                     |      "DeclarationID": "07MKKKKKKKKKKKKKK1",
                                     |      "CustomDeclarationType": "MRN",
                                     |      "DeclarationMode": "Parent Declaration",
                                     |      "ClaimDate": "20201207",
                                     |      "ClaimAmountTotal": "250000.00",
                                     |      "Claimant": "Importer",
                                     |      "PayeeIndicator": "Importer",
                                     |      "EORIsDetails": {
                                     |        "AgentEORIDetails": {
                                     |          "EORINumber": "GB070144444440921",
                                     |          "CDSFullName": "PART 575456994000 LTD",
                                     |          "LegalEntityType": "0001",
                                     |          "CDSEstablishmentAddress": {
                                     |            "City": "MALMESBURY",
                                     |            "CountryCode": "GB",
                                     |            "PostalCode": "SN17 5RP",
                                     |            "Street": "TURY HOUSE, WILTSHIRE"
                                     |          },
                                     |          "ContactInformation": {
                                     |            "City": "MALMESBURY",
                                     |            "ContactPerson": "PART 575456994000 LTD",
                                     |            "CountryCode": "GB",
                                     |            "PostalCode": "SN17 5RP",
                                     |            "Street": "TURY HOUSE, WILTSHIRE"
                                     |          },
                                     |          "VATIDsDetails": [
                                     |            {
                                     |              "CountryCode": "GB",
                                     |              "VATID": "2588684"
                                     |            }
                                     |          ]
                                     |        },
                                     |        "ImporterEORIDetails": {
                                     |          "EORINumber": "GB333333333333333",
                                     |          "CDSFullName": "PART 575456994000 LTD",
                                     |          "LegalEntityType": "0001",
                                     |          "CDSEstablishmentAddress": {
                                     |            "City": "MALMESBURY",
                                     |            "CountryCode": "GB",
                                     |            "PostalCode": "SN17 5RP",
                                     |            "Street": "TURY HOUSE, WILTSHIRE"
                                     |          },
                                     |          "ContactInformation": {
                                     |            "City": "MALMESBURY",
                                     |            "ContactPerson": "PART 575456994000 LTD",
                                     |            "CountryCode": "GB",
                                     |            "PostalCode": "SN17 5RP",
                                     |            "Street": "TURY HOUSE, WILTSHIRE"
                                     |          },
                                     |          "VATIDsDetails": [
                                     |            {
                                     |              "CountryCode": "GB",
                                     |              "VATID": "2588684"
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "MRNDetails": [
                                     |        {
                                     |          "DeclarationID": "21LLLLLLLLLLLLLLL9",
                                     |          "AcceptanceDate": "20201208",
                                     |          "DeclarantReferenceNumber": "XFGLKJDSE5GDPOIJEW985T",
                                     |          "MainDeclarationReference": "true",
                                     |          "ProcedureCode": "71",
                                     |          "BankDetails": {
                                     |            "ConsigneeBankDetails": {
                                     |              "AccountHolderName": "D Hodgson Ltd",
                                     |              "AccountNumber": "99478523",
                                     |              "SortCode": "995841"
                                     |            },
                                     |            "DeclarantBankDetails": {
                                     |              "AccountHolderName": "Angola Ltd",
                                     |              "AccountNumber": "32578523",
                                     |              "SortCode": "125822"
                                     |            }
                                     |          },
                                     |          "DeclarantDetails": {
                                     |            "EORI": "GB786958767675555",
                                     |            "LegalName": "Primitive Transport Co Ltd",
                                     |            "EstablishmentAddress": {
                                     |              "AddressLine1": "9 Strickland Drive",
                                     |              "AddressLine2": "Bare",
                                     |              "AddressLine3": "Morecambe",
                                     |              "CountryCode": "GB",
                                     |              "PostalCode": "LA4 6JU"
                                     |            },
                                     |            "ContactDetails": {
                                     |              "AddressLine1": "Jones Shipping Ltd",
                                     |              "AddressLine2": "Warehouse 22",
                                     |              "AddressLine3": "Aberdeen Docks",
                                     |              "ContactName": "Angela Jones",
                                     |              "CountryCode": "GB",
                                     |              "EmailAddress": "angelajbjones@bloggs.com",
                                     |              "PostalCode": "ABC 2DF",
                                     |              "Telephone": "017127012476"
                                     |            }
                                     |          },
                                     |          "ConsigneeDetails": {
                                     |            "EORI": "GB786958767678888",
                                     |            "LegalName": "Unique Trading Ltd",
                                     |            "EstablishmentAddress": {
                                     |              "AddressLine1": "10 Stuart Ave",
                                     |              "AddressLine2": "Bare",
                                     |              "AddressLine3": "Morecambe",
                                     |              "CountryCode": "GB",
                                     |              "PostalCode": "LA4 1RH"
                                     |            },
                                     |            "ContactDetails": {
                                     |              "AddressLine1": "Smith Transportation Ltd",
                                     |              "AddressLine2": "Smithfield",
                                     |              "AddressLine3": "London",
                                     |              "ContactName": "Rachel Smith",
                                     |              "CountryCode": "GB",
                                     |              "EmailAddress": "rachelsmith@smithtransportation.com",
                                     |              "PostalCode": "E17 2DF",
                                     |              "Telephone": "012701123476"
                                     |            }
                                     |          },
                                     |          "AccountDetails": [
                                     |            {
                                     |              "AccountType": "DAN",
                                     |              "AccountNumber": "123456781234567891",
                                     |              "EORI": "GB123456",
                                     |              "LegalName": "Unique Trading Ltd",
                                     |              "ContactDetails": {
                                     |                "ContactName": "Manikanth",
                                     |                "AddressLine1": "Smith Transportation Ltd",
                                     |                "AddressLine2": "Smithfield",
                                     |                "AddressLine3": "London",
                                     |                "PostalCode": "E27 2DF",
                                     |                "CountryCode": "AD",
                                     |                "Telephone": "012701333476",
                                     |                "EmailAddress": "klm@gmail.com"
                                     |              }
                                     |            }
                                     |          ],
                                     |          "NDRCDetails": [
                                     |            {
                                     |              "Amount": "15000.15",
                                     |              "CMAEligible": "0",
                                     |              "PaymentMethod": "001",
                                     |              "PaymentReference": "4J54KF55D5G KFJ559",
                                     |              "TaxType": "A50"
                                     |            },
                                     |            {
                                     |              "Amount": "10000.85",
                                     |              "CMAEligible": "1",
                                     |              "PaymentMethod": "001",
                                     |              "PaymentReference": "4J54KF55D5G KFJ578",
                                     |              "TaxType": "A70"
                                     |            }
                                     |          ]
                                     |        }
                                     |      ]
                                     |    }
                                     |  }
                                     |}""".stripMargin)
}
