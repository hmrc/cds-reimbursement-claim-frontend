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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq._
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import scala.collection.JavaConverters._

/** A collection of generators supporting the tests of RejectedGoodsSingleJourney. */
object SecuritiesJourneyGenerators extends JourneyGenerators with SecuritiesJourneyTestData {

  val completeJourneyGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen()

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    // acc14DeclarantMatchesUserEori: Boolean = true,
    // acc14ConsigneeMatchesUserEori: Boolean = true,
    // allDutiesCmaEligible: Boolean = true,
    // hasConsigneeDetailsInACC14: Boolean = true,
    // submitConsigneeDetails: Boolean = true,
    // submitContactDetails: Boolean = true,
    // submitContactAddress: Boolean = true,
    // submitBankAccountDetails: Boolean = true,
    // submitBankAccountType: Boolean = true,
    // reimbursementMethod: Option[ReimbursementMethodAnswer] = None
  ): Gen[SecuritiesJourney] =
    buildJourneyGen(
      // acc14DeclarantMatchesUserEori,
      // acc14ConsigneeMatchesUserEori,
      // allDutiesCmaEligible,
      // hasConsigneeDetailsInACC14,
      // submitConsigneeDetails = submitConsigneeDetails,
      // submitContactDetails = submitContactDetails,
      // submitContactAddress = submitContactAddress,
      // submitBankAccountType = submitBankAccountType,
      // submitBankAccountDetails = submitBankAccountDetails,
      // reimbursementMethod = reimbursementMethod
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete RejectedGoodsSingleJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildJourneyGen(
    // acc14DeclarantMatchesUserEori: Boolean = true,
    // acc14ConsigneeMatchesUserEori: Boolean = true,
    // allDutiesCmaEligible: Boolean = true,
    // hasConsigneeDetailsInACC14: Boolean = true,
    // submitDeclarantDetails: Boolean = true,
    // submitConsigneeDetails: Boolean = true,
    // submitContactDetails: Boolean = true,
    // submitContactAddress: Boolean = true,
    // submitBankAccountDetails: Boolean = true,
    // submitBankAccountType: Boolean = true,
    // reimbursementMethod: Option[ReimbursementMethodAnswer] = None
  ): Gen[Either[String, SecuritiesJourney]] =
    for {
      userEoriNumber          <- IdGen.genEori
      (mrn, rfs, decl)        <- mrnWithRfsWithDisplayDeclarationGen
      exportMrnAndDeclaration <- exportMrnWithDec91TrueGen
      reclaims                <- validSecurityReclaimsGen(decl)
      // declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      // consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      // numberOfTaxCodes            <- Gen.choose(1, 5)
      // taxCodes                    <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
      // paidAmounts                 <- Gen.listOfN(numberOfTaxCodes, Gen.choose[BigDecimal](BigDecimal("1.00"), BigDecimal("1000.00")))
      // reimbursementAmount         <-
      //   Gen.sequence[Seq[BigDecimal], BigDecimal](
      //     paidAmounts.map(a => Gen.choose(BigDecimal.exact("0.01"), a))
      //   )
      // basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      // methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      // reimbursementMethod         <- reimbursementMethod.map(Gen.const).getOrElse(Gen.oneOf(ReimbursementMethodAnswer.values))
      // numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
      // numberOfSupportingEvidences <- Gen.choose(0, 3)
      // numberOfDocumentTypes       <- Gen.choose(1, 2)
      // documentTypes               <- Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsSingleTypes))
      // supportingEvidences         <-
      //   Gen
      //     .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
      //       documentTypes.map(dt => Gen.choose(0, numberOfSupportingEvidences).map(n => (dt, n)))
      //     )
      //     .map(_.toMap)
      // bankAccountType             <- Gen.oneOf(BankAccountType.values)
      // consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      // declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    } yield tryBuildSecuritiesJourney(
      userEoriNumber = userEoriNumber,
      mrn = mrn,
      reasonForSecurity = rfs,
      displayDeclaration = decl,
      similarClaimExistAlreadyInCDFPay = false,
      reclaims = reclaims,
      exportMrnAndDeclaration =
        if (ReasonForSecurity.requiresExportDeclaration(rfs)) Some(exportMrnAndDeclaration) else None
      // displayDeclaration,
      // basisOfClaim,
      // "rejected goods details",
      // "special circumstances details",
      // exampleInspectionDate,
      // exampleInspectionAddress,
      // methodOfDisposal,
      // reimbursementClaims,
      // supportingEvidences,
      // if (allDutiesCmaEligible) Some(reimbursementMethod) else None,
      // declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
      // consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
      // contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
      // contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
      // bankAccountDetails =
      //   if (
      //     submitBankAccountDetails &&
      //     (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethodAnswer.BankAccountTransfer)
      //   )
      //     Some(exampleBankAccountDetails)
      //   else None,
      // bankAccountType =
      //   if (
      //     submitBankAccountType &&
      //     (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethodAnswer.BankAccountTransfer)
      //   )
      //     Some(bankAccountType)
      //   else None
    )

}
