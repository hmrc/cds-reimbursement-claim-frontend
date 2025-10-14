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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SecuritiesClaimModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

/** A collection of generators supporting the tests of SecuritiesClaim. */
object SecuritiesClaimGenerators extends ClaimGenerators with SecuritiesClaimTestData with SeqUtils {

  def validSecurityReclaimsGen(decl: ImportDeclaration): Gen[Seq[(String, TaxCode, BigDecimal, BigDecimal)]] =
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
    decl: ImportDeclaration
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

  lazy val rfsWithImportDeclarationGen: Gen[(ReasonForSecurity, ImportDeclaration)] =
    for
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (rfs, acc14)

  lazy val mrnWithRfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithImportDeclarationNotGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesImportDeclarationNotGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsWithImportDeclarationGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <-
        Gen.oneOf(
          ReasonForSecurity.values
            - ReasonForSecurity.InwardProcessingRelief
            - ReasonForSecurity.EndUseRelief
        )
      acc14 <- securitiesImportDeclarationGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithtRfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    mrnWithRfsWithImportDeclarationGen(
      ReasonForSecurity.values
        - ReasonForSecurity.InwardProcessingRelief
        - ReasonForSecurity.EndUseRelief
    )

  lazy val mrnWithtRfsWithImportDeclarationOnlyIPRGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    mrnWithRfsWithImportDeclarationGen(Set(ReasonForSecurity.InwardProcessingRelief))

  lazy val mrnWithtRfsWithImportDeclarationOnlyENUGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    mrnWithRfsWithImportDeclarationGen(Set(ReasonForSecurity.EndUseRelief))

  lazy val mrnWithtRfsWithImportDeclarationOnlyNidacGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.nidac)

  def mrnWithRfsWithImportDeclarationGen(
    reasonsForSecurity: Set[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(reasonsForSecurity)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  def mrnWithRfsWithSingleSecurityImportDeclarationGen(
    reasonsForSecurity: Set[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(reasonsForSecurity)
      acc14 <- buildSingleSecurityImportDeclarationGen(allDutiesGuaranteeEligible = false)
                 .map(
                   _.withDeclarationId(mrn.value)
                     .withDeclarantEori(exampleEori)
                     .withReasonForSecurity(rfs)
                 )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <-
        Gen.oneOf(
          TemporaryAdmissionMethodOfDisposal.values.diff(TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal)
        )
    yield (mrn, rfs, acc14, mfd)

  lazy val mrnWithRfsTempAdmissionWithImportDeclarationWithSingleShipmentMfdGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
               )
    yield (mrn, rfs, acc14, mfd)

  lazy val mrnWithRfsTempAdmissionWithImportDeclarationWithMultipleShipmentMfdGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, TemporaryAdmissionMethodOfDisposal)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.ntas)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
      mfd   <- Gen.oneOf(
                 List(TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments)
               )
    yield (mrn, rfs, acc14, mfd)

  def mrnWithRfsExcludingWithImportDeclarationGen(
    reasonsForSecurityToExclude: Set[ReasonForSecurity]
  ): Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values.filterNot(reasonsForSecurityToExclude.contains(_)))
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithIPROrENURfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf[ReasonForSecurity](ReasonForSecurity.InwardProcessingRelief, ReasonForSecurity.EndUseRelief)
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithTaRfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf[ReasonForSecurity](
                 ReasonForSecurity.TemporaryAdmission2Y,
                 ReasonForSecurity.TemporaryAdmission6M,
                 ReasonForSecurity.TemporaryAdmission3M,
                 ReasonForSecurity.TemporaryAdmission2M
               )
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithIPRRfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(ReasonForSecurity.InwardProcessingRelief)
               )
    yield (mrn, ReasonForSecurity.InwardProcessingRelief, acc14)

  lazy val mrnWithENURfsWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(ReasonForSecurity.EndUseRelief)
               )
    yield (mrn, ReasonForSecurity.EndUseRelief, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithImportDeclarationGen: Gen[(MRN, ReasonForSecurity, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      rfs   <-
        Gen.oneOf(
          ReasonForSecurity.values.filter(rfs => UploadDocumentType.securitiesDocumentTypes(rfs, None, false).isDefined)
        )
      acc14 <- securitiesImportDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    yield (mrn, rfs, acc14)

  lazy val mrnWithRfsRequiringDocumentTypeWithImportDeclarationWithDocumentTypeGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, UploadDocumentType)] =
    for
      (mrn, rfs, acc14) <- mrnWithRfsRequiringDocumentTypeWithImportDeclarationGen
      documentType      <- Gen.oneOf(UploadDocumentType.securitiesDocumentTypes(rfs, None, false).get)
    yield (mrn, rfs, acc14, documentType)

  lazy val mrnWithRfsWithImportDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithImportDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val mrnIncludingExportRfsWithImportDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithImportDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val mrnWithRfsWithImportDeclarationWithReclaimsNotGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, ImportDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])] =
    for
      (mrn, rfs, decl) <- mrnWithRfsWithImportDeclarationNotGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    yield (mrn, rfs, decl, reclaims)

  lazy val exportMrnTrueGen: Gen[MRN] =
    for mrn <- IdGen.genMRN
    yield mrn

  val completeClaimGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen()

  def completeClaimGenWithReasonsForSecurity(reasonsForSecurity: Set[ReasonForSecurity]): Gen[SecuritiesClaim] =
    buildCompleteClaimGen(reasonsForSecurity = reasonsForSecurity)

  val completeClaimWithoutIPROrENUGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
      reasonsForSecurity = ReasonForSecurity.values
        - ReasonForSecurity.InwardProcessingRelief
        - ReasonForSecurity.EndUseRelief
    )

  val completeClaimOnlyIPRGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
      reasonsForSecurity = Set(ReasonForSecurity.InwardProcessingRelief)
    )

  val completeClaimOnlyENUGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
      reasonsForSecurity = Set(ReasonForSecurity.EndUseRelief)
    )

  val completeClaimOnlyNidacGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
      reasonsForSecurity = ReasonForSecurity.nidac
    )

  val genReasonForSecurity: Gen[ReasonForSecurity] =
    Gen.oneOf(ReasonForSecurity.values)

  def buildCompleteClaimGen(
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
    reasonsForSecurity: Set[ReasonForSecurity] = ReasonForSecurity.values,
    numberOfSecurityDetails: Option[Int] = None,
    numberOfDutyTypes: Option[Int] = None
  ): Gen[SecuritiesClaim] =
    buildClaimGen(
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
      additionalDetailsVisited = true,
      numberOfSecurityDetails = numberOfSecurityDetails,
      numberOfDutyTypes = numberOfDutyTypes
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete SecuritiesClaim because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildClaimGen(
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
    additionalDetailsVisited: Boolean = false,
    numberOfSecurityDetails: Option[Int] = None,
    numberOfDutyTypes: Option[Int] = None
  ): Gen[Either[String, SecuritiesClaim]] =
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
      numberOfSecurities           = numberOfSecurityDetails match
                                       case None      => Gen.choose(2, 5).sample.get
                                       case Some(num) => num
      payeeType                   <- Gen.oneOf(PayeeType.values)
      depositsDetails             <-
        listOfExactlyN(
          numberOfSecurities,
          Gen.zip(depositIdGen, taxCodesWithAmountsGen(numberOfDutyTypes))
        )
      allDutiesGuaranteeEligible  <- allDutiesGuaranteeEligibleOpt.map(Gen.const).getOrElse(Gen.oneOf(true, false))
      acc14                        = buildSecuritiesImportDeclaration(
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
    yield {

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val correctedAmounts: SortedMap[String, SecuritiesClaim.CorrectedAmounts] =
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
        new SecuritiesClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          reasonForSecurity = Some(rfs),
          importDeclaration = Some(acc14),
          payeeType = if allDutiesGuaranteeEligible then None else Some(payeeType),
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
              || rfs == ReasonForSecurity.EndUseRelief
            then
              exampleUploadedFiles.map(
                _.copy(
                  fileName = "bod.pdf",
                  cargo = Some(
                    if rfs == ReasonForSecurity.InwardProcessingRelief
                    then UploadDocumentType.BillOfDischarge3
                    else UploadDocumentType.BillOfDischarge4
                  )
                )
              )
            else Seq.empty,
          proofOfOriginDocuments =
            if ReasonForSecurity.nidac.contains(rfs)
            then
              exampleUploadedFiles.map(
                _.copy(fileName = "proof-of-origin.pdf", cargo = Some(UploadDocumentType.ProofOfOrigin))
              )
            else Seq.empty,
          bankAccountDetails =
            if submitBankAccountDetails && (!allDutiesGuaranteeEligible) then Some(exampleBankAccountDetails)
            else None,
          bankAccountType =
            if submitBankAccountType && (!allDutiesGuaranteeEligible) then Some(bankAccountType)
            else None,
          additionalDetails = Some("additional details"),
          modes = SecuritiesClaimModes(
            checkDeclarationDetailsChangeMode = false,
            checkClaimDetailsChangeMode = true,
            checkYourAnswersChangeMode = true,
            additionalDetailsPageVisitedMode = additionalDetailsVisited
          )
        )

      SecuritiesClaim.tryBuildFrom(answers)
    }

}
