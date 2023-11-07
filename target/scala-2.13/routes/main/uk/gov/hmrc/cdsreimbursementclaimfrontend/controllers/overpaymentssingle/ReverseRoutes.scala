// @GENERATOR:play-routes-compiler
// @SOURCE:conf/overpayments_single_v2.routes

import play.api.mvc.Call


import _root_.controllers.Assets.Asset
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PathBinders._
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import _root_.uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import _root_.java.util.UUID

// @LINE:1
package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle {

  // @LINE:8
  class ReverseEnterDeclarantEoriNumberController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:8
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-declarant-eori")
    }
  
    // @LINE:9
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-declarant-eori")
    }
  
  }

  // @LINE:14
  class ReverseCheckClaimantDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:14
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details")
    }
  
    // @LINE:15
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details")
    }
  
    // @LINE:16
    def redirectToALF: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/lookup-address")
    }
  
    // @LINE:17
    def retrieveAddressFromALF(id:Option[UUID] = None): Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/update-address" + play.core.routing.queryString(List(if(id == None) None else Some(implicitly[play.api.mvc.QueryStringBindable[Option[UUID]]].unbind("id", id)))))
    }
  
  }

  // @LINE:37
  class ReverseNorthernIrelandController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:37
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claim-northern-ireland")
    }
  
    // @LINE:38
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/claim-northern-ireland")
    }
  
  }

  // @LINE:5
  class ReverseEnterImporterEoriNumberController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:5
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-importer-eori")
    }
  
    // @LINE:6
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-importer-eori")
    }
  
  }

  // @LINE:70
  class ReverseChooseFileTypeController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:70
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-file-type")
    }
  
    // @LINE:71
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-file-type")
    }
  
  }

  // @LINE:11
  class ReverseCheckDeclarationDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:11
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-declaration-details")
    }
  
    // @LINE:12
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-declaration-details")
    }
  
  }

  // @LINE:67
  class ReverseReimbursementMethodController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:67
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-repayment-method")
    }
  
    // @LINE:68
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-repayment-method")
    }
  
  }

  // @LINE:28
  class ReverseEnterImporterEoriNumberOfDuplicateDeclaration(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:28
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-importer-eori-of-duplicate-declaration")
    }
  
    // @LINE:29
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-importer-eori-of-duplicate-declaration")
    }
  
  }

  // @LINE:60
  class ReverseEnterClaimController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:60
    def showFirst: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-claim")
    }
  
    // @LINE:61
    def show(taxCode:TaxCode): Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-claim/" + play.core.routing.dynamicString(implicitly[play.api.mvc.PathBindable[TaxCode]].unbind("taxCode", taxCode)))
    }
  
    // @LINE:62
    def submit(taxCode:TaxCode): Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-claim/" + play.core.routing.dynamicString(implicitly[play.api.mvc.PathBindable[TaxCode]].unbind("taxCode", taxCode)))
    }
  
  }

  // @LINE:78
  class ReverseCheckYourAnswersController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:78
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/submit-claim")
    }
  
    // @LINE:79
    def showConfirmation: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claim-submitted")
    }
  
    // @LINE:80
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-your-answers")
    }
  
  }

  // @LINE:18
  class ReverseProblemWithAddressController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:18
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/problem-with-address")
    }
  
  }

  // @LINE:57
  class ReverseSelectDutiesController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:57
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/select-duties")
    }
  
    // @LINE:58
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/select-duties")
    }
  
  }

  // @LINE:73
  class ReverseUploadFilesController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:73
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-files")
    }
  
    // @LINE:75
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-files")
    }
  
    // @LINE:76
    def summary: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/upload-summary")
    }
  
  }

  // @LINE:34
  class ReverseCheckDuplicateDeclarationDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:34
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-duplicate-declaration-details")
    }
  
    // @LINE:35
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-duplicate-declaration-details")
    }
  
  }

  // @LINE:51
  class ReverseEnterBankAccountDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:51
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-bank-account-details")
    }
  
    // @LINE:52
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-bank-account-details")
    }
  
  }

  // @LINE:40
  class ReverseBasisForClaimController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:40
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-basis-for-claim")
    }
  
    // @LINE:41
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-basis-for-claim")
    }
  
  }

  // @LINE:46
  class ReverseChoosePayeeTypeController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:46
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-payee-type")
    }
  
    // @LINE:47
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/choose-payee-type")
    }
  
  }

  // @LINE:25
  class ReverseEnterDuplicateMovementReferenceNumberController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:25
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-duplicate-movement-reference-number")
    }
  
    // @LINE:26
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-duplicate-movement-reference-number")
    }
  
  }

  // @LINE:64
  class ReverseCheckClaimDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:64
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-claim")
    }
  
    // @LINE:65
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-claim")
    }
  
  }

  // @LINE:49
  class ReverseCheckBankDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:49
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/check-bank-details")
    }
  
  }

  // @LINE:1
  class ReverseEnterMovementReferenceNumberController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:1
    def start: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single")
    }
  
    // @LINE:2
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-movement-reference-number")
    }
  
    // @LINE:3
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-movement-reference-number")
    }
  
  }

  // @LINE:43
  class ReverseChooseBankAccountTypeController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:43
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/bank-account-type")
    }
  
    // @LINE:44
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/bank-account-type")
    }
  
  }

  // @LINE:54
  class ReverseEnterAdditionalDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:54
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-additional-details")
    }
  
    // @LINE:55
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-additional-details")
    }
  
  }

  // @LINE:20
  class ReverseEnterContactDetailsController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:20
    def show(confirmContactDetails:Boolean): Call = {
    
      (confirmContactDetails: @unchecked) match {
      
        // @LINE:20
        case (confirmContactDetails) if confirmContactDetails == false =>
          implicit lazy val _rrc = new play.core.routing.ReverseRouteContext(Map(("confirmContactDetails", false))); _rrc
          Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/change-contact-details")
      
        // @LINE:22
        case (confirmContactDetails) if confirmContactDetails == true =>
          implicit lazy val _rrc = new play.core.routing.ReverseRouteContext(Map(("confirmContactDetails", true))); _rrc
          Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/confirm-contact-details")
      
      }
    
    }
  
    // @LINE:21
    def submit(confirmContactDetails:Boolean): Call = {
    
      (confirmContactDetails: @unchecked) match {
      
        // @LINE:21
        case (confirmContactDetails) if confirmContactDetails == false =>
          implicit lazy val _rrc = new play.core.routing.ReverseRouteContext(Map(("confirmContactDetails", false))); _rrc
          Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/change-contact-details")
      
        // @LINE:23
        case (confirmContactDetails) if confirmContactDetails == true =>
          implicit lazy val _rrc = new play.core.routing.ReverseRouteContext(Map(("confirmContactDetails", true))); _rrc
          Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/claimant-details/confirm-contact-details")
      
      }
    
    }
  
  }

  // @LINE:31
  class ReverseEnterDeclarantEoriNumberOfDuplicateDeclarationController(_prefix: => String) {
    def _defaultPrefix: String = {
      if (_prefix.endsWith("/")) "" else "/"
    }

  
    // @LINE:31
    def show: Call = {
      
      Call("GET", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-declarant-eori-of-duplicate-declaration")
    }
  
    // @LINE:32
    def submit: Call = {
      
      Call("POST", _prefix + { _defaultPrefix } + "overpayments/v1/single/enter-declarant-eori-of-duplicate-declaration")
    }
  
  }


}
