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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq._
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._

/** A collection of generators supporting the tests of RejectedGoodsSingleJourney. */
@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object RejectedGoodsSingleJourneyGenerators extends RejectedGoodsSingleJourneyTestData {

  val completeJourneyWithMatchingUserEoriAndAllDutiesCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen()

  val completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(allDutiesCmaEligible = false)

  val completeJourneyAllDutiesCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false,
      allDutiesCmaEligible = false
    )

  val completeJourneyGen: Gen[RejectedGoodsSingleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndAllDutiesCMAEligibleGen,
      completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen,
      completeJourneyAllDutiesCMAEligibleGen,
      completeJourneyNotCMAEligibleGen
    )

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true
  ): Gen[RejectedGoodsSingleJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesCmaEligible,
      hasConsigneeDetailsInACC14
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
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true
  ): Gen[Either[String, RejectedGoodsSingleJourney]] =
    for {
      id                          <- Gen.uuid
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes            <- Gen.choose(1, 5)
      taxCodes                    <- Gen.const(TaxCodes.all.take(numberOfTaxCodes))
      paidAmounts                 <- Gen.listOfN(numberOfTaxCodes, Gen.choose(1d, 10000d)).map(_.map(BigDecimal.apply(_)))
      reimbursementAmount         <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(0.01d, a.toDouble).map(BigDecimal.apply))
        )
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.all)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.all)
      reimbursementMethod         <- Gen.oneOf(ReimbursementMethodAnswer.all)
      numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
      numberOfSupportingEvidences <- Gen.choose(1, 2)
      documentTypes               <- Gen.listOfN(numberOfSupportingEvidences, Gen.oneOf(DocumentTypeRejectedGoods.all))
      bankAccountType             <- Gen.oneOf(BankAccountType.allAccountTypes)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)]          =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

      val reimbursementClaims: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.take(numberOfSelectedTaxCodes).zip(reimbursementAmount).map { case (t, a) =>
          (t, a, allDutiesCmaEligible)
        }

      val supportingEvidences                    =
        documentTypes.zipWithIndex.map { case (d, i) => (s"evidence-$i", d) }

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          id.toString(),
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties
        )

      tryBuildRejectedGoodsSingleJourney(
        userEoriNumber,
        mrn,
        displayDeclaration,
        basisOfClaim,
        "details of rejected goods",
        "special circumstances details",
        exampleInspectionDate,
        exampleInspectionAddress,
        methodOfDisposal,
        reimbursementClaims,
        supportingEvidences,
        if (allDutiesCmaEligible) Some(reimbursementMethod) else None,
        declarantEoriNumber =
          if (submitDeclarantDetails && !acc14DeclarantMatchesUserEori) Some(declarantEORI) else None,
        consigneeEoriNumber =
          if (submitConsigneeDetails && !acc14ConsigneeMatchesUserEori) Some(consigneeEORI) else None,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        bankAccountDetails =
          if (
            submitBankAccountDetails &&
            (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethodAnswer.BankAccountTransfer)
          )
            Some(exampleBankAccountDetails)
          else None,
        bankAccountType =
          if (
            submitBankAccountType &&
            (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethodAnswer.BankAccountTransfer)
          )
            Some(bankAccountType)
          else None
      )
    }

  val displayDeclarationCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = true)

  val displayDeclarationNotCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = false)

  val displayDeclarationGen: Gen[DisplayDeclaration] =
    Gen.oneOf(
      displayDeclarationCMAEligibleGen,
      displayDeclarationNotCMAEligibleGen
    )

  val exampleDisplayDeclaration: DisplayDeclaration =
    displayDeclarationGen.sample.get

  def buildDisplayDeclarationGen(cmaEligible: Boolean): Gen[DisplayDeclaration] =
    for {
      id               <- Gen.uuid
      declarantEORI    <- IdGen.genEori
      consigneeEORI    <- IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.const(TaxCodes.all.take(numberOfTaxCodes))
      paidAmounts      <- Gen.listOfN(numberOfTaxCodes, Gen.choose(1, 10000)).map(_.map(BigDecimal.apply(_)))
    } yield {
      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, cmaEligible) }
      buildDisplayDeclaration(id.toString(), declarantEORI, Some(consigneeEORI), paidDuties)
    }

}
