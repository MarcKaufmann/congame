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

  up.compiler("form", function(el) {
    el.addEventListener("submit", function(e) {
      var ok = true;
      el.querySelectorAll("input[type=file]").forEach(function(inputEl) {
        var files = inputEl.files;
        for (var i = 0; i < files.length; i++) {
          var file = files[i];
          if (file.size > 15 * 1024 * 1024) {
            ok = false;
            alert("Uploaded files must be less than 15MB in size.");
          }
        }
      });

      if (ok) {
        return true;
      }
      e.preventDefault();
      return false;
    });
  });
})();
