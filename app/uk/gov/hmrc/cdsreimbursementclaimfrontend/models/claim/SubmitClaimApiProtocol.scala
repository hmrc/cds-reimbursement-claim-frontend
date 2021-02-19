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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim

import java.time.Instant

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimApiProtocol.FileInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimRequest.{EntryDetails, GoodsDetails, MrnDetails, TPI05RequestDetail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CompleteC285Claim, SignedInUserDetails}
import cats.syntax.eq._
import scala.math.BigDecimal.RoundingMode

//This protocol is used when the frontend wants to send down to this service for the finel submission.
final case class SubmitClaimApiProtocol(
  tpi05RequestDetail: TPI05RequestDetail,
  files: List[FileInformation]
)

object SubmitClaimApiProtocol {

  implicit val fileInformationFormat: OFormat[FileInformation]            = Json.format
  implicit val frontendSubmitClaimFormat: OFormat[SubmitClaimApiProtocol] = Json.format

  final case class FileInformation(uploadReference: UploadReference, documentType: String)

//TODO None of this is verified, it's just me, Gabor, looking at specs. When it's ready we have to go through field by field!!!!!!!!!
  def fromCompleteC285Claim(claim: CompleteC285Claim, userDetails: SignedInUserDetails): SubmitClaimApiProtocol = {

    val ukClaims         = claim.maybeUKDuty.map(_.dutyAmounts.map(_.claim.toList).flatten.sum)
    val euClaims         = claim.maybeEUDuty.map(_.dutyAmounts.map(_.claim.toList).flatten.sum)
    val claimAmountTotal =
      ukClaims.getOrElse(euClaims.getOrElse(BigDecimal(0))) //TODO This will definitely needs some refinements

    val basis2 = claim.maybeReasonForClaimAndBasisAnswer.map(_.basisForClaim.repr)
    val basis1 = claim.maybeReasonForClaim.map(_.repr)

    val claimant = if (claim.declarantType.declarantType.repr === "Importer") "Importer" else "Representative"

    //EntryDeclarationDetails
    val goodsDetails = GoodsDetails(
      placeOfImport = claim.declarationDetails.flatMap(_.declarationDetail.map(_.placeOfImport)),
      isPrivateImporter = None, //TODO
      groundsForRepaymentApplication = None, //TODO
      descOfGoods = None //TODO
    )

    val mrnDetails = MrnDetails( //TODO
      MRNNumber = None,
      acceptanceDate = None,
      declarantReferenceNumber = None,
      mainDeclarationReference = Some(true),
      procedureCode = None,
      declarantDetails = None,
      accountDetails = None,
      consigneeDetails = None,
      bankInfo = None,
      NDRCDetails = None
    )

    val entryDetails = EntryDetails( //TODO
      entryNumber = None,
      entryDate = None,
      declarantReferenceNumber = None,
      mainDeclarationReference = Some(true),
      declarantDetails = None,
      accountDetails = None,
      consigneeDetails = None,
      bankInfo = None,
      NDRCDetails = None
    )

    val tpi05RequestDetail =
      TPI05RequestDetail(
        CDFPayService = Some("NDRC"),
        dateReceived = Some(SubmitClaimRequest.dateFormatter.format(Instant.now())),
        claimType = Some("C285"), //Later can become C&1179
        caseType = Some("Individual"),
        customDeclarationType = Some(claim.movementReferenceNumber.fold(_ => "Entry", _ => "MRN")),
        declarationMode = Some("Parent Declaration"),
        claimDate = Some(SubmitClaimRequest.dateFormatter.format(Instant.now())),
        claimAmountTotal = Some(claimAmountTotal.setScale(2, RoundingMode.HALF_UP).toString()), //TODO Is this OK?
        disposalMethod = None, //Mandatory in case of C&1179, in case of C285 it is optional
        reimbursementMethod = Some("Bank Transfer"), //When CMA (Current Month Adjustment) changes to Deferment
        basisOfClaim = if (basis1.isDefined) basis1 else basis2, //TODO is this OK?
        claimant = Some(claimant),
        payeeIndicator = None, //TODO Mikail where is this coming from?
        newEORI = None, //Only for securities
        newDAN = None, //Only for securities
        authorityTypeProvided = None, //Only for CMA (Current Month Adjustment)
        claimantEORI = Some(userDetails.eori.value), //The person who's making the claim
        claimantEmailAddress = userDetails.email.map(_.value), //The person who's making the claim
        goodsDetails = Some(goodsDetails),
        EORIDetails = None,
        MRNDetails = Some(List(mrnDetails)),
        //DuplicateMRNDetails = None,
        entryDetails = Some(List(entryDetails))
        //duplicateEntryDetails = None
      )
    val files              = Nil //TODO Get the list of files, and their filetypes

    SubmitClaimApiProtocol(tpi05RequestDetail, files)
  }

}
