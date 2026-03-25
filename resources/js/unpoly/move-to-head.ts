import up from "./unpoly";

up.compiler("[data-move-to-head]", function (el: HTMLInputElement) {
  const id = el.dataset.moveToHead as string;
  let found = false;
  document.head.querySelectorAll("link").forEach((linkEl) => {
    if (linkEl.id === id) {
      found = true;
    }
  });
  if (found) return;

  el.id = id;
  el.parentNode.removeChild(el);
  document.head.appendChild(el);
});
