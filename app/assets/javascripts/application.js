window.GOVUKFrontend.initAll();
window.HMRCFrontend.initAll();


// Functionality for CDSR service

const CDSR = {

    form: document.querySelector('form'),
    errorSummary: document.querySelector('#cdsr-dynamic-error-summary'),
    errorPageTitle: "Error: " + document.title,
    errorInputElement: document.querySelector('#file-upload-error'),
    errorsShowing: false,

    Init: () => {

        CDSR.form.addEventListener('submit', CDSR.CheckFileInputs);

    },

    CheckFileInputs: (event) => {

        let fileInputs = document.querySelectorAll('input[type=file]');

        for (i = 0; i < fileInputs.length; ++i) {
            if (fileInputs[i].files.length == 0) {

                event.preventDefault();

                if (CDSR.errorsShowing === false) {
                    CDSR.errorSummary.classList.remove('govuk-!-display-none');
                    CDSR.errorSummary.querySelector('a').focus();

                    let errorMessaging = CDSR.errorInputElement.cloneNode(true);
                    CDSR.errorInputElement.remove();
                    fileInputs[i].parentNode.insertBefore(errorMessaging, fileInputs[i]);

                    fileInputs[i].parentNode.classList.add('govuk-form-group--error');

                    document.title = CDSR.errorPageTitle;

                    CDSR.errorsShowing = true;
                }

            }
        }

    }

}

window.addEventListener('load', CDSR.Init);


// =====================================================
// Back link mimics browser back functionality
// =====================================================
// store referrer value to cater for IE - https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/10474810/  */
const docReferrer = document.referrer

// prevent resubmit warning
if (window.history && window.history.replaceState && typeof window.history.replaceState === 'function') {
  window.history.replaceState(null, null, window.location.href);
}

//;(function(window, document) {
////  GOVUK.details.init();
//
//  var errorSummary = document.querySelector('.error-summary');
//  if (errorSummary) {
//    errorSummary.focus();
//  }
//
////  var stepByStepNavigation = new GOVUK.Modules.StepByStepNavigation()
////  const stepBySteps = document.querySelectorAll('.app-step-nav')
////  if (stepBySteps.length) {
////    stepBySteps.forEach(function (s) {
////      stepByStepNavigation.start($(s))
////    })
////  }
//
//  var countryEl = document.querySelector("#countryCode");
////  var lang = GOVUK.getCookie("PLAY_LANG")
//  if (countryEl && (!lang || lang === "en")) {
//    openregisterLocationPicker({
//      selectElement: countryEl,
//      name: 'countryCode-name',
//      url: '/claim-for-reimbursement-of-import-duties/assets/location-autocomplete-graph.json',
//      defaultValue: ''
//    });
//
//    var wrapper = document.querySelector('.country-code-wrapper');
//
//    function resetSelectIfEmpty(e) {
//      if (e.target.id === 'countryCode') {
//        var val = e.target.value.trim();
//        var countrySelect = document.querySelector("#countryCode-select");
//        if (countrySelect) {
//          var countriesArray = Array.prototype.slice.call(countrySelect.options);
//          var matches = countriesArray.filter(function (o) {
//            return o.text !== '' && o.text === val
//          });
//          if (!matches.length) {
//            countrySelect.value = ''
//          }
//        }
//      }
//    }
//
//    wrapper.addEventListener('change', resetSelectIfEmpty);
//  }
//
////  var urBanner = document.querySelector('.ur-banner')
////  if (urBanner) {
////    var urBannerCookieName = 'mtdpurr'
////    var hasDismissed = GOVUK.getCookie(urBannerCookieName)
////
////    function removeUrBanner () {
////      urBanner.parentNode.removeChild(urBanner)
////    }
////
////    function dismissUrBanner (e) {
////      if (e.target && e.target.id === 'dismiss-ur-banner') {
////        e.preventDefault()
////        GOVUK.setCookie(urBannerCookieName, 'suppress_for_all_services', { days: 30 })
////        removeUrBanner()
////      }
////    }
////
////    function showUrBanner () {
////      urBanner.addEventListener('click', dismissUrBanner)
////      urBanner.classList.remove('js-hidden')
////    }
////
////    if (hasDismissed) {
////      removeUrBanner()
////    } else {
////      showUrBanner()
////    }
////  }
//
////  if (window.jsConfig && window.jsConfig.timeoutEnabled) {
////    GOVUK.sessionTimeout({
////      timeout: window.jsConfig.timeout,
////      countdown: window.jsConfig.countdown,
////      keep_alive_url: window.jsConfig.keep_alive_url,
////      logout_url: window.jsConfig.logout_url,
////      timed_out_url: window.jsConfig.timed_out_url
////    })
////  }
//
//})(window, document);
