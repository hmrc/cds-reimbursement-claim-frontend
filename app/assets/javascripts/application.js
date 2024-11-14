// Functionality for CDSR service

const CDSR = {

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