import up from "./unpoly";

up.compiler("[data-track-timings]", function (el: HTMLElement) {
  const params = new URL(window.location.href).searchParams;
  const totalStart = time() - Number(params.get("__tt")) * 1;
  let focusStart = time() - Number(params.get("__ft")) * 1;
  let focusTotal = 0;

  el.querySelectorAll("a.button").forEach((el) => {
    el.addEventListener("click", onButtonClick);
  });
  el.querySelectorAll("form").forEach((el) => {
    el.addEventListener("submit", onFormSubmit);
  });
  document.addEventListener("visibilitychange", onVisibilityChange);
  return function () {
    el.querySelectorAll("a.button").forEach((el) => {
      el.removeEventListener("click", onButtonClick);
    });
    el.querySelectorAll("form").forEach((el) => {
      el.removeEventListener("submit", onFormSubmit);
    });
    document.removeEventListener("visibilitychange", onVisibilityChange);
  };

  function time() {
    return new Date().getTime();
  }

  function enrich(urlString: string) {
    const tt = (time() - totalStart).toString();
    const ft = (time() - focusStart + focusTotal).toString();
    const url = new URL(urlString);
    url.searchParams.set("__tt", tt);
    url.searchParams.set("__ft", ft);
    return url.toString();
  }

  function onButtonClick(e: any) {
    e.target.href = enrich(e.target.href);
    return true;
  }

  function onFormSubmit(e: any) {
    e.target.action = enrich(e.target.action);
    return true;
  }

  function onVisibilityChange() {
    if (document.visibilityState === "visible") {
      focusStart = time();
    } else {
      focusTotal += time() - focusStart;
    }
  }
});
