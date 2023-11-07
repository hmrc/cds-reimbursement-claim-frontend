// @GENERATOR:play-routes-compiler
// @SOURCE:conf/overpayments_single_v2.routes

import play.api.routing.JavaScriptReverseRoute


import _root_.controllers.Assets.Asset
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PathBinders._
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import _root_.java.util.UUID

// @LINE:1
package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.javascript {

  // @LINE:8
  class ReverseEnterDeclarantEoriNumberController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:8
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterDeclarantEoriNumberController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-declarant-eori"})
        }
      """
    )
  
    // @LINE:9
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterDeclarantEoriNumberController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-declarant-eori"})
        }
      """
    )
  
  }

  // @LINE:14
  class ReverseCheckClaimantDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:14
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimantDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details"})
        }
      """
    )
  
    // @LINE:15
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimantDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details"})
        }
      """
    )
  
    // @LINE:16
    def redirectToALF: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimantDetailsController.redirectToALF",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/lookup-address"})
        }
      """
    )
  
    // @LINE:17
    def retrieveAddressFromALF: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimantDetailsController.retrieveAddressFromALF",
      """
        function(id0) {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/update-address" + _qS([(id0 == null ? null : (""" + implicitly[play.api.mvc.QueryStringBindable[Option[UUID]]].javascriptUnbind + """)("id", id0))])})
        }
      """
    )
  
  }

  // @LINE:37
  class ReverseNorthernIrelandController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:37
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.NorthernIrelandController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claim-northern-ireland"})
        }
      """
    )
  
    // @LINE:38
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.NorthernIrelandController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claim-northern-ireland"})
        }
      """
    )
  
  }

  // @LINE:5
  class ReverseEnterImporterEoriNumberController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:5
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterImporterEoriNumberController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-importer-eori"})
        }
      """
    )
  
    // @LINE:6
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterImporterEoriNumberController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-importer-eori"})
        }
      """
    )
  
  }

  // @LINE:70
  class ReverseChooseFileTypeController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:70
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ChooseFileTypeController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-file-type"})
        }
      """
    )
  
    // @LINE:71
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ChooseFileTypeController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-file-type"})
        }
      """
    )
  
  }

  // @LINE:11
  class ReverseCheckDeclarationDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:11
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckDeclarationDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-declaration-details"})
        }
      """
    )
  
    // @LINE:12
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckDeclarationDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-declaration-details"})
        }
      """
    )
  
  }

  // @LINE:67
  class ReverseReimbursementMethodController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:67
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ReimbursementMethodController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-repayment-method"})
        }
      """
    )
  
    // @LINE:68
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ReimbursementMethodController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-repayment-method"})
        }
      """
    )
  
  }

  // @LINE:28
  class ReverseEnterImporterEoriNumberOfDuplicateDeclaration(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:28
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterImporterEoriNumberOfDuplicateDeclaration.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-importer-eori-of-duplicate-declaration"})
        }
      """
    )
  
    // @LINE:29
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterImporterEoriNumberOfDuplicateDeclaration.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-importer-eori-of-duplicate-declaration"})
        }
      """
    )
  
  }

  // @LINE:60
  class ReverseEnterClaimController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:60
    def showFirst: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterClaimController.showFirst",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-claim"})
        }
      """
    )
  
    // @LINE:61
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterClaimController.show",
      """
        function(taxCode0) {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-claim/" + encodeURIComponent((""" + implicitly[play.api.mvc.PathBindable[TaxCode]].javascriptUnbind + """)("taxCode", taxCode0))})
        }
      """
    )
  
    // @LINE:62
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterClaimController.submit",
      """
        function(taxCode0) {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-claim/" + encodeURIComponent((""" + implicitly[play.api.mvc.PathBindable[TaxCode]].javascriptUnbind + """)("taxCode", taxCode0))})
        }
      """
    )
  
  }

  // @LINE:78
  class ReverseCheckYourAnswersController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:78
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckYourAnswersController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/submit-claim"})
        }
      """
    )
  
    // @LINE:79
    def showConfirmation: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckYourAnswersController.showConfirmation",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claim-submitted"})
        }
      """
    )
  
    // @LINE:80
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckYourAnswersController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-your-answers"})
        }
      """
    )
  
  }

  // @LINE:18
  class ReverseProblemWithAddressController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:18
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ProblemWithAddressController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/problem-with-address"})
        }
      """
    )
  
  }

  // @LINE:57
  class ReverseSelectDutiesController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:57
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.SelectDutiesController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/select-duties"})
        }
      """
    )
  
    // @LINE:58
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.SelectDutiesController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/select-duties"})
        }
      """
    )
  
  }

  // @LINE:73
  class ReverseUploadFilesController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:73
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.UploadFilesController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-files"})
        }
      """
    )
  
    // @LINE:75
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.UploadFilesController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-files"})
        }
      """
    )
  
    // @LINE:76
    def summary: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.UploadFilesController.summary",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/upload-summary"})
        }
      """
    )
  
  }

  // @LINE:34
  class ReverseCheckDuplicateDeclarationDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:34
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckDuplicateDeclarationDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-duplicate-declaration-details"})
        }
      """
    )
  
    // @LINE:35
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckDuplicateDeclarationDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-duplicate-declaration-details"})
        }
      """
    )
  
  }

  // @LINE:51
  class ReverseEnterBankAccountDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:51
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterBankAccountDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-bank-account-details"})
        }
      """
    )
  
    // @LINE:52
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterBankAccountDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-bank-account-details"})
        }
      """
    )
  
  }

  // @LINE:40
  class ReverseBasisForClaimController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:40
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.BasisForClaimController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-basis-for-claim"})
        }
      """
    )
  
    // @LINE:41
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.BasisForClaimController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-basis-for-claim"})
        }
      """
    )
  
  }

  // @LINE:46
  class ReverseChoosePayeeTypeController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:46
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ChoosePayeeTypeController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-payee-type"})
        }
      """
    )
  
    // @LINE:47
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ChoosePayeeTypeController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/choose-payee-type"})
        }
      """
    )
  
  }

  // @LINE:25
  class ReverseEnterDuplicateMovementReferenceNumberController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:25
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterDuplicateMovementReferenceNumberController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-duplicate-movement-reference-number"})
        }
      """
    )
  
    // @LINE:26
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.EnterDuplicateMovementReferenceNumberController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-duplicate-movement-reference-number"})
        }
      """
    )
  
  }

  // @LINE:64
  class ReverseCheckClaimDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:64
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-claim"})
        }
      """
    )
  
    // @LINE:65
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckClaimDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-claim"})
        }
      """
    )
  
  }

  // @LINE:49
  class ReverseCheckBankDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:49
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.CheckBankDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/check-bank-details"})
        }
      """
    )
  
  }

  // @LINE:1
  class ReverseEnterMovementReferenceNumberController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:1
    def start: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterMovementReferenceNumberController.start",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single"})
        }
      """
    )
  
    // @LINE:2
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterMovementReferenceNumberController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-movement-reference-number"})
        }
      """
    )
  
    // @LINE:3
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterMovementReferenceNumberController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-movement-reference-number"})
        }
      """
    )
  
  }

  // @LINE:43
  class ReverseChooseBankAccountTypeController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:43
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.ChooseBankAccountTypeController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/bank-account-type"})
        }
      """
    )
  
    // @LINE:44
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.ChooseBankAccountTypeController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/bank-account-type"})
        }
      """
    )
  
  }

  // @LINE:54
  class ReverseEnterAdditionalDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:54
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterAdditionalDetailsController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-additional-details"})
        }
      """
    )
  
    // @LINE:55
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterAdditionalDetailsController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-additional-details"})
        }
      """
    )
  
  }

  // @LINE:20
  class ReverseEnterContactDetailsController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:20
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterContactDetailsController.show",
      """
        function(confirmContactDetails0) {
        
          if (confirmContactDetails0 == """ + implicitly[play.api.mvc.JavascriptLiteral[Boolean]].to(false) + """) {
            return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/change-contact-details"})
          }
        
          if (confirmContactDetails0 == """ + implicitly[play.api.mvc.JavascriptLiteral[Boolean]].to(true) + """) {
            return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/confirm-contact-details"})
          }
        
        }
      """
    )
  
    // @LINE:21
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterContactDetailsController.submit",
      """
        function(confirmContactDetails0) {
        
          if (confirmContactDetails0 == """ + implicitly[play.api.mvc.JavascriptLiteral[Boolean]].to(false) + """) {
            return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/change-contact-details"})
          }
        
          if (confirmContactDetails0 == """ + implicitly[play.api.mvc.JavascriptLiteral[Boolean]].to(true) + """) {
            return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/claimant-details/confirm-contact-details"})
          }
        
        }
      """
    )
  
  }

  // @LINE:31
  class ReverseEnterDeclarantEoriNumberOfDuplicateDeclarationController(_prefix: => String) {

    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:31
    def show: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterDeclarantEoriNumberOfDuplicateDeclarationController.show",
      """
        function() {
          return _wA({method:"GET", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-declarant-eori-of-duplicate-declaration"})
        }
      """
    )
  
    // @LINE:32
    def submit: JavaScriptReverseRoute = JavaScriptReverseRoute(
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v1.EnterDeclarantEoriNumberOfDuplicateDeclarationController.submit",
      """
        function() {
          return _wA({method:"POST", url:"""" + _prefix + { _defaultPrefix } + """" + "overpayments/v1/single/enter-declarant-eori-of-duplicate-declaration"})
        }
      """
    )
  
  }


}
