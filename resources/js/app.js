/* global Sentry, up*/
(function() {
  Sentry.onLoad(function() {
    var pidEl = document.querySelector("[data-participant-id]");
    if (pidEl) {
      Sentry.setUser({
        id: pidEl.dataset.participantId
      });
    }
  });

  up.compiler(".toggleable__toggle", function(el) {
    el.addEventListener("click", function(e) {
      var parentEl = e.target.closest(".toggleable");
      parentEl.classList.toggle("toggleable--hidden");
    });
  });
})();
