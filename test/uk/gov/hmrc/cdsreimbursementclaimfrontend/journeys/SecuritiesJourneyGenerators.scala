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

import cats.implicits.catsSyntaxEq
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import scala.collection.JavaConverters._

/** A collection of generators supporting the tests of SecuritiesJourney. */
object SecuritiesJourneyGenerators extends JourneyGenerators with SecuritiesJourneyTestData {

  def validSecurityReclaimsGen(decl: DisplayDeclaration): Gen[Seq[(String, TaxCode, BigDecimal)]] =
    Gen
      .sequence(
        decl.getSecurityDepositIds
          .getOrElse(Seq.empty)
          .halfNonEmpty
          .flatMap(depositId =>
            decl
              .getSecurityDetailsFor(depositId)
              .map(sd =>
                sd.taxDetails.halfNonEmpty.map(td =>
                  Gen
                    .choose(BigDecimal.exact("0.01"), td.getAmount)
                    .map(amount => (sd.securityDepositId, td.getTaxCode, amount))
                )
              )
              .getOrElse(Seq.empty)
          )
      )
      .map(_.asScala.toSeq)

  def validSecurityReclaimsFullAmountGen(decl: DisplayDeclaration): Gen[Seq[(String, TaxCode, BigDecimal)]] =
    Gen
      .const(
        decl.getSecurityDepositIds
          .getOrElse(Seq.empty)
          .flatMap(depositId =>
            decl
              .getSecurityDetailsFor(depositId)
              .map(sd => sd.taxDetails.map(td => (sd.securityDepositId, td.getTaxCode, td.getAmount)))
              .getOrElse(Seq.empty)
          )
      )

  lazy val rfsWithDisplayDeclarationGen: Gen[(ReasonForSecurity, DisplayDeclaration)] =
    for {
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationNotGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationNotGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithtRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  def mrnWithRfsWithDisplayDeclarationGen(
    reasonsForSecurity: Seq[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(reasonsForSecurity)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithRfsTempAdmissionWithDisplayDeclarationWithMfdGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.temporaryAdmissions)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 TemporaryAdmissionMethodOfDisposal.values.filterNot(
                   _ === TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
                 )
               )
    } yield (mrn, rfs, acc14, mfd)

  lazy val mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.temporaryAdmissions)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
               )
    } yield (mrn, rfs, acc14, mfd)

  def mrnWithRfsExcludingWithDisplayDeclarationGen(
    reasonsForSecurityToExclude: Seq[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values.filterNot(reasonsForSecurityToExclude.contains(_)))
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithIprOrErRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf[ReasonForSecurity](ReasonForSecurity.InwardProcessingRelief, ReasonForSecurity.EndUseRelief)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values.filter(rfs => UploadDocumentType.securitiesTypes(rfs).isDefined))
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationWithDocumentTypeGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, UploadDocumentType)] =
    for {
      (mrn, rfs, acc14) <- mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen
      documentType      <- Gen.oneOf(UploadDocumentType.securitiesTypes(rfs).get)
    } yield (mrn, rfs, acc14, documentType)

  lazy val mrnWithRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  lazy val mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  lazy val mrnWithRfsWithDisplayDeclarationWithReclaimsNotGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationNotGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  lazy val exportMrnTrueGen: Gen[MRN] =
    for {
      mrn <- IdGen.genMRN
    } yield mrn

  val completeJourneyGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen()

  val genReasonForSecurity: Gen[ReasonForSecurity] =
    Gen.oneOf(ReasonForSecurity.values)

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
    submitBankAccountType: Boolean = true,
    submitFullAmount: Boolean = false
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
      submitBankAccountDetails = submitBankAccountDetails,
      submitFullAmount = submitFullAmount
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
    submitBankAccountType: Boolean = true,
    submitFullAmount: Boolean = false
  ): Gen[Either[String, SecuritiesJourney]] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      rfs                         <- genReasonForSecurity
      methodOfDisposal            <-
        if (ReasonForSecurity.temporaryAdmissions.contains(rfs))
          Gen.oneOf(TemporaryAdmissionMethodOfDisposal.values).map(Some.apply)
        else Gen.const(None)
      exportMrn                   <-
        if (
          ReasonForSecurity.temporaryAdmissions(rfs) && methodOfDisposal.contains(
            TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
          )
        ) exportMrnTrueGen.map(Some.apply)
        else
          Gen.const(None)
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
      numberOfSecurities          <- Gen.choose(2, 5)
      reclaimsDetails             <-
        listOfExactlyN(
          numberOfSecurities,
          Gen.zip(depositIdGen, taxCodesWithAmountsGen)
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
      reclaims                    <- if (submitFullAmount) validSecurityReclaimsFullAmountGen(acc14) else validSecurityReclaimsGen(acc14)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        listOfExactlyN(
          numberOfDocumentTypes,
          Gen.oneOf(UploadDocumentType.securitiesTypes(rfs).getOrElse(Seq(UploadDocumentType.SupportingEvidence)))
        )
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
        exportMrn,
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
        supportingEvidences = supportingEvidences,
        methodOfDisposal = methodOfDisposal
      )
    }

}
