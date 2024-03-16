import up from "./unpoly";

up.compiler("[data-mask]", function (el: HTMLInputElement) {
  let form = el.closest("form");
  let groups = form.querySelectorAll("[data-mask-group]");

  el.addEventListener("change", function () {
    reset();
  });

  reset();
  return;

  function reset() {
    let active = el.value;
    groups.forEach(function (g: HTMLElement) {
      if (g.dataset.maskGroup === active) {
        g.style.display = "block";
      } else {
        g.style.display = "none";
      }
    });
  }
});
