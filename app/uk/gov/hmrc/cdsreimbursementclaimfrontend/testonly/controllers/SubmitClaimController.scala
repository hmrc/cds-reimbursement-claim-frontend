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

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}

@Singleton
class SubmitClaimController @Inject() ()(implicit
  mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with Logging {

  val claim: Action[AnyContent] = Action {
    Ok("temporary solution")
//    (request.method match {
//      case "GET"  => rightT[Future, Error](testRequestBody)
//      case "POST" => fromOption[Future](request.body.asJson, Error("Request Body is not Json!"))
//    })
//      .flatMap(j => claimService.submitClaim(Json.fromJson[Subj, Lang.defaultLang))
//      .fold(
//        e => {
//          logger.warn(s"could not submit claim", e)
//          InternalServerError
//        },
//        response => Ok(Json.toJson(response))
//      )

  }

  val testRequestBody: JsValue = Json.parse(
    """
      |{
      |  "postNewClaimsRequest": {
      |    "requestCommon": {
      |      "originatingSystem": "ABCDEFGHIJKLMNOPQRST",
      |      "receiptDate": "1374-89-42T57:57:33Z",
      |      "acknowledgementReference": "ABCDEFGHIJKL"
      |    },
      |    "requestDetail": {
      |      "CDFPayService": "NDRC",
      |      "dateReceived": "86842966",
      |      "claimType": "C285",
      |      "caseType": "CMA",
      |      "customDeclarationType": "Entry",
      |      "declarationMode": "Parent Declaration",
      |      "claimDate": "39198584",
      |      "claimAmountTotal": "-3.5",
      |      "disposalMethod": "Donation to Charity",
      |      "reimbursementMethod": "Bank Transfer",
      |      "basisOfClaim": "Incorrect CPC",
      |      "claimant": "Representative",
      |      "payeeIndicator": "Importer",
      |      "newEORI": "NGTY",
      |      "newDAN": "ABCD",
      |      "authorityTypeProvided": "StndAuth",
      |      "claimantEORI": "AB12345678901234Z",
      |      "claimantEmailAddress": "ABCDEFGHI",
      |      "goodsDetails": {
      |        "placeOfImport": "ABCDEFGHIJKLMN",
      |        "isPrivateImporter": "No",
      |        "groundsForRepaymentApplication": "Duties no provided",
      |        "descOfGoods": "ABCDEFGHIJKLMNOPQRSTUVWXYZA"
      |      },
      |      "EORIDetails": {
      |        "agentEORIDetails": {
      |          "EORINumber": "API",
      |          "CDSEstablishmentAddress": {
      |            "contactPerson": "ABCDE",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRSTU",
      |            "addressline2": "ABCDEFGHIJKLMNOPQRST",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "street": "ABCDEFGHIJKLMNOP",
      |            "city": "ABCDEFGHIJKLMNOPQ",
      |            "countryCode": "CD",
      |            "postalCode": "ABCD",
      |            "telephone": "ABCDEFGHIJKLMNOPQRSTUVWXYZA",
      |            "emailAddress": "ABCDEFGHI"
      |          },
      |          "CDSFullName": "ABCDEFGHIJKLM",
      |          "legalEntityType": "ABCDEFGHIJKLMNOPQRSTUVW",
      |          "EORIStartDate": "58833441",
      |          "EORIEndDate": "73234980",
      |          "contactInformation": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRSTUVWXY",
      |            "addressline1": "ABCDEFGH",
      |            "addressline2": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "street": "ABCDEFGHI",
      |            "city": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "countryCode": "UN",
      |            "postalCode": "ABCDE",
      |            "telephoneNumber": "ABCDEFGHIJKLMNOPQRSTUVWXYZABC",
      |            "faxNumber": "ABCDEFGHIJKLM",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB"
      |          },
      |          "VATDetails": []
      |        },
      |        "importerEORIDetails": {
      |          "EORINumber": "ZO5",
      |          "CDSEstablishmentAddress": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "addressline1": "ABCDEFGHIJKLMNOPQ",
      |            "addressline2": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUVWXYZA",
      |            "street": "ABCDEFGHIJKLMNOPQRSTUVWX",
      |            "city": "ABCDEFGHIJKLMNOPQRST",
      |            "countryCode": "SX",
      |            "postalCode": "ABCDEF",
      |            "telephone": "ABCDEFGHIJKLMNOPQRS",
      |            "emailAddress": "ABCDEFG"
      |          },
      |          "CDSFullName": "ABCDEFGHIJKLMNOPQRSTU",
      |          "legalEntityType": "ABCDEFGHIJKLMNOPQRSTUVWXY",
      |          "EORIStartDate": "58535854",
      |          "EORIEndDate": "38347206",
      |          "contactInformation": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQ",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRSTU",
      |            "addressline2": "ABCDEFGHIJKLMNOPQRST",
      |            "addressline3": "ABCDEFGHI",
      |            "street": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "city": "ABCDEFGHIJKLM",
      |            "countryCode": "FR",
      |            "postalCode": "ABCDE",
      |            "telephoneNumber": "ABCDEFGHIJKLM",
      |            "faxNumber": "ABCDEF",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      |          },
      |          "VATDetails": []
      |        }
      |      },
      |      "MRNDetails": [],
      |      "DuplicateMRNDetails": {
      |        "MRNNumber": "ABCDEFGHIJKLM",
      |        "acceptanceDate": "89755830",
      |        "declarantReferenceNumber": "ABCDEFGHIJKLM",
      |        "mainDeclarationReference": false,
      |        "procedureCode": "A",
      |        "declarantDetails": {
      |          "EORI": "FM6UQW",
      |          "legalName": "ABCDEFGHIJKLMNOPQRSTUVWXYZABC",
      |          "establishmentAddress": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRSTUVWXYZABC",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRSTUV",
      |            "addressline2": "ABCDEF",
      |            "addressline3": "ABCDEF",
      |            "street": "ABCD",
      |            "city": "ABCDEFGHIJK",
      |            "countryCode": "BL",
      |            "postalCode": "ABCD",
      |            "telephone": "ABCDEFGHIJKLMNOPQRST",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWX"
      |          },
      |          "contactDetails": {
      |            "contactPerson": "ABCDEFGHIJKLMNO",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRST",
      |            "addressline2": "ABCDEFGHIJ",
      |            "addressline3": "ABCDEFGHI",
      |            "street": "ABCDEFGH",
      |            "city": "ABCDEFGHIJKLMNOPQRSTUVWX",
      |            "countryCode": "GS",
      |            "postalCode": "ABCDEF",
      |            "telephoneNumber": "ABCDEFGHIJKLMNOPQRSTUVW",
      |            "faxNumber": "ABCDEFGH",
      |            "emailAddress": "ABCDEF"
      |          }
      |        },
      |        "accountDetails": [],
      |        "consigneeDetails": {
      |          "EORI": "QJY",
      |          "legalName": "ABCDEFGH",
      |          "establishmentAddress": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "addressline1": "ABCDEFGHIJKLMNOP",
      |            "addressline2": "ABCDEFGHIJKLM",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "street": "ABCDEFGHIJKLMNO",
      |            "city": "ABCDEFGHIJKLMNOPQRSTUVWX",
      |            "countryCode": "PS",
      |            "postalCode": "ABCDEFG",
      |            "telephone": "ABCDEFGHIJKLMNOPQRSTUVWX",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB"
      |          },
      |          "contactDetails": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRSTUVW",
      |            "addressline1": "ABCDEFGHIJK",
      |            "addressline2": "ABCDEFGHIJKLMNOP",
      |            "addressline3": "ABCDEFGHIJKLMNOPQR",
      |            "street": "ABCDEFG",
      |            "city": "ABCDEFGHIJKLMNOP",
      |            "countryCode": "TH",
      |            "postalCode": "ABCDEFGH",
      |            "telephoneNumber": "ABCDEFGHI",
      |            "faxNumber": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "emailAddress": "ABCDE"
      |          }
      |        },
      |        "bankInfo": {
      |          "consigneeBankDetails": {
      |            "accountHolderName": "ABCDEFGHI",
      |            "sortCode": "ABCD",
      |            "accountNumber": "ABCDE"
      |          },
      |          "declarantBankDetails": {
      |            "accountHolderName": "ABCDEFGHIJ",
      |            "sortCode": "ABCDE",
      |            "accountNumber": "ABCDEFG"
      |          }
      |        },
      |        "NDRCDetails": []
      |      },
      |      "entryDetails": [],
      |      "duplicateEntryDetails": {
      |        "entryNumber": "ABCDEFGH",
      |        "entryDate": "09151384",
      |        "declarantReferenceNumber": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |        "mainDeclarationReference": false,
      |        "declarantDetails": {
      |          "EORI": "GGQG",
      |          "legalName": "ABCDEFGHIJK",
      |          "establishmentAddress": {
      |            "contactPerson": "ABCDEFGHIJKLMNO",
      |            "addressline1": "ABCDEFGHIJKL",
      |            "addressline2": "ABCDEFGHIJKL",
      |            "addressline3": "ABCD",
      |            "street": "ABCDEFGHIJKLMNOPQR",
      |            "city": "ABCDEFGHIJKLMNOPQR",
      |            "countryCode": "FJ",
      |            "postalCode": "ABCDEFG",
      |            "telephone": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB"
      |          },
      |          "contactDetails": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRS",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRSTUVWXYZABC",
      |            "addressline2": "ABCDEF",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUV",
      |            "street": "ABCDEFGHIJKL",
      |            "city": "ABCDEFG",
      |            "countryCode": "GP",
      |            "postalCode": "ABCDE",
      |            "telephoneNumber": "ABCDEFGHIJKLMNOPQ",
      |            "faxNumber": "ABCDEF",
      |            "emailAddress": "ABCDEFGHIJKLMNOPQRSTUVWXYZA"
      |          }
      |        },
      |        "accountDetails": [],
      |        "consigneeDetails": {
      |          "EORI": "QPY5W87",
      |          "legalName": "ABCDEFGHIJKLMNOPQRS",
      |          "establishmentAddress": {
      |            "contactPerson": "ABCDEFGHIJKLMNOPQRS",
      |            "addressline1": "ABCDEFGHIJKLMNOPQRSTUVWXYZA",
      |            "addressline2": "ABCDEFGHIJKLMN",
      |            "addressline3": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "street": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      |            "city": "ABCDEFGHI",
      |            "countryCode": "KI",
      |            "postalCode": "ABCDEFG",
      |            "telephone": "ABCDEFGHIJKLMNOPQRSTUVWXYZAB",
      |            "emailAddress": "ABCDEFGHIJKLM"
      |          },
      |          "contactDetails": {
      |            "contactPerson": "ABCDEFGHIJ",
      |            "addressline1": "ABCDEFGH",
      |            "addressline2": "ABCDEFGHIJKLMNOPQRSTUVWXY",
      |            "addressline3": "ABCDEFGHI",
      |            "street": "ABCDEFG",
      |            "city": "ABCDE",
      |            "countryCode": "US",
      |            "postalCode": "ABCDEF",
      |            "telephoneNumber": "ABCDEFGHIJKL",
      |            "faxNumber": "ABCDEFGHIJKLMNOPQR",
      |            "emailAddress": "ABCDEFGHIJ"
      |          }
      |        },
      |        "bankInfo": {
      |          "consigneeBankDetails": {
      |            "accountHolderName": "ABCDEFGHIJKLMNOPQRS",
      |            "sortCode": "ABCDE",
      |            "accountNumber": "ABCDE"
      |          },
      |          "declarantBankDetails": {
      |            "accountHolderName": "ABCDEFGHIJKLMNOPQRSTUVW",
      |            "sortCode": "ABCDE",
      |            "accountNumber": "ABCDEF"
      |          }
      |        },
      |        "NDRCDetails": []
      |      }
      |    }
      |  }
      |}
      |""".stripMargin
  )

}
