import up from "./unpoly";

up.compiler("[data-counter]", function (el: HTMLInputElement) {
  let prefix = el.dataset.counterPrefix || "";
  let min = el.min && Number(el.min) * 100;
  let max = el.max && Number(el.max) * 100;
  let step = Number(el.step) * 100;
  let value = min || 0;
  let group = el.closest(".group");
  let minusBtn = makeButton("-", () => {
    value -= step;
    if (min !== undefined && value < min) {
      value = min;
    }
  });
  let plusBtn = makeButton("+", () => {
    value += step;
    if (max !== undefined && value > max) {
      value = max;
    }
  });
  let container = document.createElement("div");
  container.classList.add("counter");
  let label = document.createElement("span");
  el.closest("label").style.display = "none";
  container.appendChild(minusBtn);
  container.appendChild(label);
  container.appendChild(plusBtn);
  group.appendChild(container);
  update();
  return () => {
    minusBtn.parentNode.removeChild(minusBtn);
    plusBtn.parentNode.removeChild(plusBtn);
  };

  function update() {
    const formatter = Intl.NumberFormat(undefined, {
      minimumFractionDigits: 2,
    });
    const finalValue = value / 100;
    el.value = `${finalValue}`;
    label.innerText = `${prefix}${formatter.format(finalValue)}`;
  }

  function makeButton(label: string, cb: (event: Event) => void) {
    let elt = document.createElement("button");
    elt.type = "button";
    elt.innerText = label;
    elt.addEventListener("click", (e) => {
      cb(e);
      update();
      return false;
    });
    return elt;
  }
});
