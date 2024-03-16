import up from "./unpoly";

up.compiler("form", function (el: HTMLElement) {
  el.addEventListener("submit", function (e: Event) {
    let ok = true;
    el.querySelectorAll("input[type=file]").forEach(function (
      inputEl: HTMLInputElement,
    ) {
      let files = inputEl.files;
      for (let i = 0; i < files.length; i++) {
        let file = files[i];
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
