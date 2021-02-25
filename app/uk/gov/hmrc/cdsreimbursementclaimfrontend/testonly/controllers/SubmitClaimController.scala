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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.testonly.controllers

import play.api.i18n.Lang
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class SubmitClaimController @Inject() ()(implicit
  mcc: MessagesControllerComponents,
  claimService: ClaimService
) extends FrontendController(mcc)
    with Logging {

  val claim: Action[AnyContent] = Action.async { implicit request =>
    claimService
      .testSubmitClaim(tpi05Payload, Lang.defaultLang)
      .fold(
        e => {
          logger.warn("submission failed ", e)
          InternalServerError
        },
        s => Ok(Json.toJson(s))
      )

  }

  val tpi05Payload: JsValue = Json.parse(
    """
      |{
      |    "postNewClaimsRequest": {
      |        "requestCommon": {
      |            "originatingSystem": "MDTP",
      |            "receiptDate": "2021-01-01T00:00:00Z",
      |            "acknowledgementReference": "TTT-21099922299191119999"
      |        },
      |        "requestDetail": {
      |            "CDFPayService": "NDRC",
      |            "dateReceived": "20210211",
      |            "claimType": "C285",
      |            "caseType": "Individual",
      |            "customDeclarationType": "MRN",
      |            "declarationMode": "Parent Declaration",
      |            "claimDate": "20210101",
      |            "claimAmountTotal": "2600.09",
      |            "disposalMethod": "Donation to Charity",
      |            "reimbursementMethod": "Bank Transfer",
      |            "basisOfClaim": "Incorrect Value",
      |            "claimant": "Representative",
      |            "payeeIndicator": "Representative",
      |            "newEORI": "GB1234",
      |            "newDAN": "1234",
      |            "authorityTypeProvided": "StndAuth",
      |            "claimantEORI": "GB777777777777777",
      |            "claimantEmailAddress": "Claimant@a.com",
      |            "goodsDetails": {
      |                "placeOfImport": "Sample Place of Import",
      |                "isPrivateImporter": "Yes",
      |                "groundsForRepaymentApplication": "Special circumstances",
      |                "descOfGoods": "Sample description of goods"
      |            },
      |            "EORIDetails": {
      |                "agentEORIDetails": {
      |                    "EORINumber": "GB123",
      |                    "CDSFullName": "PART 575456994000 LTD",
      |                    "legalEntityType": "0001",
      |                    "EORIStartDate": "20201001",
      |                    "EORIEndDate": "20231003",
      |                    "CDSEstablishmentAddress": {
      |                        "street": "TURY HOUSE, WILTSHIRE",
      |                        "city": "MALMESBURY",
      |                        "countryCode": "GB",
      |                        "postalCode": "SN17 5RP",
      |                        "telephone": "987654321",
      |                        "emailAddress": "test@gmail.com"
      |                    },
      |                    "contactInformation": {
      |                        "contactPerson": "MRs Smith",
      |                        "street": "5 Sland Road, Nowhere",
      |                        "city": "Leeds",
      |                        "countryCode": "GB",
      |                        "postalCode": "YU4 13KL",
      |                        "telephoneNumber": "987654322",
      |                        "faxNumber": "23456789",
      |                        "emailAddress": "test3@gmail.com"
      |                    }
      |                },
      |                "importerEORIDetails": {
      |                    "EORINumber": "GB123",
      |                    "CDSFullName": "PART 07010921000 LTD",
      |                    "legalEntityType": "0001",
      |                    "EORIStartDate": "20201001",
      |                    "EORIEndDate": "20231001",
      |                    "CDSEstablishmentAddress": {
      |                        "street": "TURY HOUSE, WILTSHIRE",
      |                        "city": "MALMESBURY",
      |                        "countryCode": "GB",
      |                        "postalCode": "SN18 5RP",
      |                        "telephone": "987654325",
      |                        "emailAddress": "test@gmail.com"
      |                    },
      |                    "contactInformation": {
      |                        "contactPerson": "MRs Smith",
      |                        "street": "5 Sland Road, Nowhere",
      |                        "city": "Leeds",
      |                        "countryCode": "GB",
      |                        "postalCode": "YU4 13KL",
      |                        "telephoneNumber": "987654322",
      |                        "faxNumber": "23456789",
      |                        "emailAddress": "test3@gmail.com"
      |                    }
      |                }
      |            },
      |            "MRNDetails": [
      |                {
      |                    "MRNNumber": "11aaaaaaaaaaaaaaa1",
      |                    "acceptanceDate": "20210101",
      |                    "declarantReferenceNumber": "XFGLKJDSE5GDPOIJEW985T",
      |                    "mainDeclarationReference": true,
      |                    "procedureCode": "71",
      |                    "declarantDetails": {
      |                        "EORI": "GB123",
      |                        "legalName": "PART 575456994000 LTD",
      |                        "establishmentAddress": {
      |                            "addressLine1": "TURY HOUSE",
      |                            "addressLine2": "WILTSHIRE",
      |                            "AddressLine3": "MALMESBURY",
      |                            "countryCode": "GB",
      |                            "postalCode": "SN17 5RP"
      |                        },
      |                        "contactDetails": {
      |                            "contactPerson": "MRs Smith",
      |                            "addressLine1": "5 Sland Road",
      |                            "addressLine2": "Nowhere",
      |                            "addressLine3": "Leeds",
      |                            "countryCode": "GB",
      |                            "postalCode": "YU4 13KL",
      |                            "telephoneNumber": "987654322",
      |                            "faxNumber": "23456789",
      |                            "emailAddress": "test3@gmail.com"
      |                        }
      |                    },
      |                    "accountDetails": [
      |                        {
      |                            "accountType": "001",
      |                            "accountNumber": "12345678912345",
      |                            "EORI": "GB2345",
      |                            "legalName": "Test",
      |                            "contactDetails": {
      |                                "contactPerson": "Siri Smith",
      |                                "addressLine1": "5 Sland Road",
      |                                "addressLine2": "Nowhere",
      |                                "addressLine3": "Leeds",
      |                                "countryCode": "GB",
      |                                "telephoneNumber": "987654321",
      |                                "emailAddress": "test@gmail.com"
      |                            }
      |                        },
      |                        {
      |                            "accountType": "003",
      |                            "accountNumber": "12345679",
      |                            "EORI": "GB2345",
      |                            "legalName": "Test",
      |                            "contactDetails": {
      |                                "contactPerson": "Siri Smith",
      |                                "addressLine1": "5 Sland Road",
      |                                "addressLine2": "Nowhere",
      |                                "addressLine3": "Leeds",
      |                                "countryCode": "GB",
      |                                "telephoneNumber": "987654321",
      |                                "emailAddress": "test@gmail.com"
      |                            }
      |                        }
      |                    ],
      |                    "consigneeDetails": {
      |                        "EORI": "GB123",
      |                        "legalName": "consignee",
      |                        "establishmentAddress": {
      |                            "addressLine1": "TURY HOUSE",
      |                            "addressLine2": "WILTSHIRE",
      |                            "AddressLine3": "MALMESBURY",
      |                            "countryCode": "GB",
      |                            "postalCode": "SN17 5RP"
      |                        },
      |                        "contactDetails": {
      |                            "contactPerson": "MRs Smith",
      |                            "addressLine1": "5 Sland Road",
      |                            "addressLine2": "Nowhere",
      |                            "addressLine3": "Leeds",
      |                            "countryCode": "GB",
      |                            "postalCode": "YU4 13KL",
      |                            "telephoneNumber": "987654322",
      |                            "faxNumber": "23456789",
      |                            "emailAddress": "test3@gmail.com"
      |                        }
      |                    },
      |                    "bankDetails": {
      |                        "consigneeBankDetails": {
      |                            "accountHolderName": "Consignee name",
      |                            "sortCode": "123456",
      |                            "accountNumber": "12345678"
      |                        },
      |                        "declarantBankDetails": {
      |                            "accountHolderName": "Declarant Name",
      |                            "sortCode": "123457",
      |                            "accountNumber": "12345679"
      |                        }
      |                    },
      |                    "NDRCDetails": [
      |                        {
      |                            "paymentMethod": "001",
      |                            "paymentReference": "Some Ref String",
      |                            "CMAEligible": "0",
      |                            "taxType": "A90",
      |                            "amount": "500.00",
      |                            "claimAmount": "100.00"
      |                        },
      |                        {
      |                            "paymentMethod": "001",
      |                            "paymentReference": "Ref1",
      |                            "CMAEligible": "0",
      |                            "taxType": "A30",
      |                            "amount": "1500.01",
      |                            "claimAmount": "1000.00"
      |                        },
      |                        {
      |                            "paymentMethod": "003",
      |                            "paymentReference": "Another Ref String",
      |                            "CMAEligible": "0",
      |                            "taxType": "B00",
      |                            "amount": "2500.00",
      |                            "claimAmount": "1500.63"
      |                        }
      |                    ]
      |                }
      |            ]
      |        }
      |    }
      |}
      |""".stripMargin
  )

}
