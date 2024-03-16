import up from "./unpoly";

up.compiler(".toggleable__toggle", function (el: HTMLElement) {
  el.addEventListener("click", function (e: Event) {
    var parentEl = (e.target as HTMLElement).closest(".toggleable");
    parentEl.classList.toggle("toggleable--hidden");
  });
});
