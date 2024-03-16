import Sentry from "./sentry";

if (Sentry !== undefined) {
  Sentry.onLoad(function () {
    var pidEl: HTMLElement = document.querySelector("[data-participant-id]");
    if (pidEl) {
      Sentry.setUser({
        id: pidEl.dataset.participantId,
      });
    }
  });
}
