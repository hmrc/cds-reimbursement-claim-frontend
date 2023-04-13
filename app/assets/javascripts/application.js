// Functionality for CDSR service

const CDSR = {

    form: document.querySelector('form'),
    lang: document.documentElement.lang,

    errorSummary: document.querySelector('#cdsr-dynamic-error-summary'),
    errorPageTitle: "Error: " + document.title,
    errorInputElement: document.querySelector('#file-upload-error'),
    errorsShowing: false,

    backLinkAnchor: document.querySelector('.govuk-back-link'),
    skipLinkAnchor: document.querySelector('.govuk-skip-link'),

    Init: function () {
        // Add back link
        if (CDSR.backLinkAnchor) {
            CDSR.backLinkAnchor.addEventListener('click', CDSR.NavigateBack);
        }

        // Fix back link after skip link usage
        if (CDSR.skipLinkAnchor) {
            CDSR.skipLinkAnchor.addEventListener('click', CDSR.NavigateSkip);
        }

        // Initialise file upload check
        if (CDSR.form) {
            CDSR.form.addEventListener('submit', CDSR.CheckFileInputs);
        }

        // Open feedback link in new window
        const feedbackLink = document.querySelector('.govuk-phase-banner a.govuk-link');

        if (feedbackLink) {
            feedbackLink.setAttribute('target', '_blank');
        }

    },

    NavigateBack: function (event) {
        event.preventDefault();
        history.back();
    },

    NavigateSkip: function (event) {
        event.preventDefault();
        document.getElementById("main-content").scrollIntoView();
    },

    CheckFileInputs: function (event) {

        var fileInputs = document.querySelectorAll('input[type=file]');

        for (i = 0; i < fileInputs.length; ++i) {
            if (fileInputs[i].files.length == 0) {

                event.preventDefault();

                if (CDSR.errorsShowing === false) {
                    CDSR.errorSummary.classList.remove('govuk-!-display-none');

                    var errorMessaging = CDSR.errorInputElement.cloneNode(true);
                    CDSR.errorInputElement.remove();
                    fileInputs[i].parentNode.insertBefore(errorMessaging, fileInputs[i]);

                    fileInputs[i].parentNode.classList.add('govuk-form-group--error');

                    document.title = CDSR.errorPageTitle;

                    CDSR.errorsShowing = true;
                }

                CDSR.errorSummary.querySelector('a').focus();

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