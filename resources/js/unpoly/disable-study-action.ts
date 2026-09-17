import up from "./unpoly";

up.compiler("[data-study-action]", function (el: HTMLElement) {
  let fired = false;
  switch (el.tagName) {
    case "A":
      el.addEventListener("up:link:follow", ensureButtonOneshot);
      break;
    case "FORM":
      el.addEventListener("up:form:submit", ensureFormOneshot);
      break;
  }
  return () => {
    switch (el.tagName) {
      case "A":
        el.removeEventListener("up:link:follow", ensureButtonOneshot);
        break;
      case "FORM":
        el.removeEventListener("up:form:submit", ensureFormOneshot);
        break;
    }
  };

  function ensureButtonOneshot(e: Event) {
    if (fired) {
      console.warn("[data-study-action]: button click prevented");
      e.preventDefault();
      return;
    }
    fired = true;
  }

  function ensureFormOneshot(e: Event) {
    if (fired) {
      console.warn("[data-study-action]: form submit prevented");
      e.preventDefault();
      return;
    }
    fired = true;
    el.querySelectorAll("button[type=submit]").forEach(
      (buttonEl: HTMLButtonElement) => {
        buttonEl.disabled = true;
      },
    );
  }
});
