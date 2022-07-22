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

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import java.util.Locale

/** A collection of generators supporting the tests of SecuritiesJourney. */
object SecuritiesJourneyGenerators extends JourneyGenerators with SecuritiesJourneyTestData {

  val completeJourneyGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen()

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = false,
    allDutiesGuaranteeEligibleOpt: Option[Boolean] = None,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true
  ): Gen[SecuritiesJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesGuaranteeEligibleOpt,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete SecuritiesJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = false,
    allDutiesGuaranteeEligibleOpt: Option[Boolean] = None,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true
  ): Gen[Either[String, SecuritiesJourney]] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      rfs                         <- Gen.oneOf(ReasonForSecurity.values)
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
      numberOfSecurities          <- Gen.choose(2, 5)
      reclaimsDetails             <-
        listOfExactlyN(
          numberOfSecurities,
          Gen.zip(
            listOfExactlyN(7, Gen.alphaNumChar).map(l => String.valueOf(l.toArray).toUpperCase(Locale.ENGLISH)),
            taxCodesWithAmountsGen
          )
        )
      allDutiesGuaranteeEligible  <- allDutiesGuaranteeEligibleOpt.map(Gen.const(_)).getOrElse(Gen.oneOf(true, false))
      acc14                        = buildSecuritiesDisplayDeclaration(
                                       id = mrn.value,
                                       securityReason = rfs.acc14Code,
                                       declarantEORI = declarantEORI,
                                       consigneeEORI = if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
                                       reclaimsDetails = reclaimsDetails,
                                       allDutiesGuaranteeEligible = allDutiesGuaranteeEligible,
                                       consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
                                       declarantContact = declarantContact
                                     )
      exportMrn                   <- exportMrnTrueGen
      reclaims                    <- validSecurityReclaimsGen(acc14)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <- listOfExactlyN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsSingleTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
    } yield {

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      tryBuildSecuritiesJourney(
        userEoriNumber = userEoriNumber,
        mrn = mrn,
        reasonForSecurity = rfs,
        displayDeclaration = acc14,
        similarClaimExistAlreadyInCDFPay = false,
        reclaims = reclaims,
        exportMrn =
          if (ReasonForSecurity.requiresExportDeclaration(rfs)) Some(exportMrn)
          else
            None,
        declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
        consigneeEoriNumber =
          if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI)
          else None,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        bankAccountDetails =
          if (submitBankAccountDetails && (!allDutiesGuaranteeEligible))
            Some(exampleBankAccountDetails)
          else None,
        bankAccountType =
          if (submitBankAccountType && (!allDutiesGuaranteeEligible))
            Some(bankAccountType)
          else None,
        supportingEvidences = supportingEvidences
      )
    }

  val mrnWithRfsWithDisplayDeclarationWithReclaimsGen = for {
    (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGen
    reclaims         <- validSecurityReclaimsGen(decl)
  } yield (mrn, rfs, decl, reclaims)

}
