/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SecuritiesJourneyModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

/** A collection of generators supporting the tests of SecuritiesJourney. */
object SecuritiesJourneyGenerators extends JourneyGenerators with SecuritiesJourneyTestData with SeqUtils {

  def validSecurityReclaimsGen(decl: DisplayDeclaration): Gen[Seq[(String, TaxCode, BigDecimal, BigDecimal)]] =
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
                    .choose(BigDecimal.exact("0.00"), td.getAmount - BigDecimal.exact("0.01"))
                    .map(amount => (sd.securityDepositId, td.getTaxCode, td.getAmount, amount))
                )
              )
              .getOrElse(Seq.empty)
          )
      )
      .map(_.asScala.toSeq)

  def validSecurityReclaimsFullAmountGen(
    decl: DisplayDeclaration
  ): Gen[Seq[(String, TaxCode, BigDecimal, BigDecimal)]] =
    Gen
      .const(
        decl.getSecurityDepositIds
          .getOrElse(Seq.empty)
          .flatMap(depositId =>
            decl
              .getSecurityDetailsFor(depositId)
              .map(sd =>
                sd.taxDetails.map(td => (sd.securityDepositId, td.getTaxCode, td.getAmount, BigDecimal("0.00")))
              )
              .getOrElse(Seq.empty)
          )
      )

  lazy val rfsWithDisplayDeclarationGen: Gen[(ReasonForSecurity, DisplayDeclaration)] =
    for
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationNotGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationNotGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief)
      acc14 <- securitiesDisplayDeclarationGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithtRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithtRfsWithDisplayDeclarationWithoutIPRGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    mrnWithRfsWithDisplayDeclarationGen(ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief)

  lazy val mrnWithtRfsWithDisplayDeclarationOnlyIPRGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    mrnWithRfsWithDisplayDeclarationGen(Set(ReasonForSecurity.InwardProcessingRelief))

  def mrnWithRfsWithDisplayDeclarationGen(
    reasonsForSecurity: Set[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(reasonsForSecurity)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsTempAdmissionWithDisplayDeclarationWithMfdGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <-
        Gen.oneOf(
          TemporaryAdmissionMethodOfDisposal.values.diff(TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal)
        )
    yield (mrn, rfs, acc14, mfd)

  lazy val mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
               )
    yield (mrn, rfs, acc14, mfd)

  lazy val mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 List(TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments)
               )
    yield (mrn, rfs, acc14, mfd)

  def mrnWithRfsExcludingWithDisplayDeclarationGen(
    reasonsForSecurityToExclude: Set[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values.filterNot(reasonsForSecurityToExclude.contains(_)))
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithIprOrEurRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf[ReasonForSecurity](ReasonForSecurity.InwardProcessingRelief, ReasonForSecurity.EndUseRelief)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithTaRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf[ReasonForSecurity](
                 ReasonForSecurity.TemporaryAdmission2Y,
                 ReasonForSecurity.TemporaryAdmission6M,
                 ReasonForSecurity.TemporaryAdmission3M,
                 ReasonForSecurity.TemporaryAdmission2M
               )
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithIprRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(ReasonForSecurity.InwardProcessingRelief)
               )
    yield (mrn, ReasonForSecurity.InwardProcessingRelief, acc14)

  lazy val mrnWithEurRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(ReasonForSecurity.EndUseRelief)
               )
    yield (mrn, ReasonForSecurity.EndUseRelief, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <-
        Gen.oneOf(
          ReasonForSecurity.values.filter(rfs => UploadDocumentType.securitiesDocumentTypes(rfs, None, false).isDefined)
        )
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationWithDocumentTypeGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, UploadDocumentType)] =
    for
      (mrn, rfs, acc14) <- mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen
      documentType      <- Gen.oneOf(UploadDocumentType.securitiesDocumentTypes(rfs, None, false).get)
    yield (mrn, rfs, acc14, documentType)

  lazy val mrnWithRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val mrnWithRfsWithDisplayDeclarationWithReclaimsNotGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationNotGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val exportMrnTrueGen: Gen[MRN] =
    for mrn <- IdGen.genMRN
    yield mrn

  val completeJourneyGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen()

  def completeJourneyGenWithReasonsForSecurity(reasonsForSecurity: Set[ReasonForSecurity]): Gen[SecuritiesJourney] =
    buildCompleteJourneyGen(reasonsForSecurity = reasonsForSecurity)

  val completeJourneyWithoutIPRGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen(
      reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief
    )

  val completeJourneyOnlyIPRGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen(
      reasonsForSecurity = Set(ReasonForSecurity.InwardProcessingRelief)
    )

  val genReasonForSecurity: Gen[ReasonForSecurity] =
    Gen.oneOf(ReasonForSecurity.values)

  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = false,
    allDutiesGuaranteeEligibleOpt: Option[Boolean] = Some(false),
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    submitFullAmount: Boolean = false,
    reasonsForSecurity: Set[ReasonForSecurity] = ReasonForSecurity.values
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
      submitFullAmount = submitFullAmount,
      reasonsForSecurity = reasonsForSecurity,
      additionalDetailsVisited = true
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
    submitFullAmount: Boolean = false,
    reasonsForSecurity: Set[ReasonForSecurity] = ReasonForSecurity.values,
    additionalDetailsVisited: Boolean = false
  ): Gen[Either[String, SecuritiesJourney]] =
    for
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      rfs                         <- Gen.oneOf(reasonsForSecurity)
      methodOfDisposal            <-
        if ReasonForSecurity.ntas.contains(rfs) then
          Gen.nonEmptyListOf(Gen.oneOf(TemporaryAdmissionMethodOfDisposal.selectableValues)).map(Some.apply)
        else Gen.const(None)
      exportMrns                  <-
        if ReasonForSecurity.ntas(rfs) && methodOfDisposal
            .exists(mods => TemporaryAdmissionMethodOfDisposal.containsExportedMethodsOfDisposal(mods))
        then exportMrnTrueGen.map(mrn => Some(Seq(mrn)))
        else Gen.const(None)
      declarantEORI               <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
      numberOfSecurities          <- Gen.choose(2, 5)
      payeeType                   <- Gen.oneOf(PayeeType.values)
      depositsDetails             <-
        listOfExactlyN(
          numberOfSecurities,
          Gen.zip(depositIdGen, taxCodesWithAmountsGen)
        )
      allDutiesGuaranteeEligible  <- allDutiesGuaranteeEligibleOpt.map(Gen.const).getOrElse(Gen.oneOf(true, false))
      acc14                        = buildSecuritiesDisplayDeclaration(
                                       id = mrn.value,
                                       securityReason = rfs.acc14Code,
                                       declarantEORI = declarantEORI,
                                       consigneeEORI = if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
                                       depositDetails = depositsDetails,
                                       allDutiesGuaranteeEligible = allDutiesGuaranteeEligible,
                                       consigneeContact = if submitConsigneeDetails then consigneeContact else None,
                                       declarantContact = declarantContact
                                     )
      reclaims                    <-
        if submitFullAmount then validSecurityReclaimsFullAmountGen(acc14) else validSecurityReclaimsGen(acc14)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        listOfExactlyN(
          numberOfDocumentTypes,
          Gen.oneOf(
            UploadDocumentType
              .securitiesDocumentTypes(rfs, methodOfDisposal, false)
              .getOrElse(Seq(UploadDocumentType.SupportingEvidence))
          )
        )
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      additionalDetailsPageVisited = additionalDetailsVisited
    yield {

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val correctedAmounts: SortedMap[String, SecuritiesJourney.CorrectedAmounts] =
        SortedMap(
          reclaims
            .groupBy(_._1)
            .view
            .mapValues(s => SortedMap(s.map { case (_, taxCode, _, amount) => (taxCode, Option(amount)) }*))
            .toSeq*
        )

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        new SecuritiesJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          reasonForSecurity = Some(rfs),
          displayDeclaration = Some(acc14),
          payeeType = Some(payeeType),
          similarClaimExistAlreadyInCDFPay = Some(false),
          eoriNumbersVerification = eoriNumbersVerification,
          exportMovementReferenceNumbers = exportMrns,
          temporaryAdmissionMethodsOfDisposal = methodOfDisposal,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          correctedAmounts = Some(correctedAmounts),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
          billOfDischargeDocuments =
            if rfs == ReasonForSecurity.InwardProcessingRelief
            then
              exampleUploadedFiles.map(_.copy(fileName = "bod.pdf", cargo = Some(UploadDocumentType.BillOfDischarge3)))
            else Seq.empty,
          bankAccountDetails =
            if submitBankAccountDetails && (!allDutiesGuaranteeEligible) then Some(exampleBankAccountDetails)
            else None,
          bankAccountType =
            if submitBankAccountType && (!allDutiesGuaranteeEligible) then Some(bankAccountType)
            else None,
          additionalDetails = Some("additional details"),
          modes = SecuritiesJourneyModes(
            checkDeclarationDetailsChangeMode = false,
            checkClaimDetailsChangeMode = true,
            checkYourAnswersChangeMode = true,
            additionalDetailsPageVisitedMode = additionalDetailsPageVisited
          )
        )

      SecuritiesJourney.tryBuildFrom(answers)
    }

}
