import up from "./unpoly";

up.compiler("[data-calibration]", function (el: HTMLInputElement) {
  let prefix = el.dataset.calibrationPrefix || "£";
  let min = el.min && Number(el.min) * 100;
  let max = el.max && Number(el.max) * 100;
  let value = 200;
  let counter = 0;
  let group = el.closest(".group"); // FIXME: Needs to change for Lioness
  let formatter = Intl.NumberFormat(undefined, {
    minimumFractionDigits: 2,
  });
  let confirmChoice = document.getElementById("confirm-choice");
  confirmChoice.hidden = true;
  let confirmationMessage = document.getElementById("confirmation-message");
  let resetBtn = document.getElementById("reset-button");
  resetBtn.addEventListener("click", () => {
    reset();
    return false;
  });

  let optionALabel = `Option A: decode 7 sequences for ${prefix}${formatter.format(value / 100)}`;
  let optionABtn = makeButton(optionALabel, () => {
    if (counter < 5) {
      max = value;
      value = (value + min) / 2;
      value = Math.round(value / 10) * 10;
      counter += 1;
    }
  });

  let optionBLabel = `Option B: decode 0 sequences for ${prefix}0.00`;
  let optionBBtn = makeButton(optionBLabel, () => {
    if (counter < 5) {
      min = value;
      value = (value + max) / 2;
      value = Math.round(value / 10) * 10;
      counter += 1;
    }
  });

  let container = document.createElement("div");
  container.classList.add("calibration");
  let label = document.createElement("span");
  el.closest("label").style.display = "none";
  container.appendChild(optionABtn);
  container.appendChild(label);
  container.appendChild(optionBBtn);
  group.appendChild(container);
  update();
  return () => {
    optionABtn.parentNode.removeChild(optionABtn);
    optionBBtn.parentNode.removeChild(optionBBtn);
  };

  function update() {
    const finalValue = value / 100;
    el.value = `${finalValue}`;
    label.innerText = `${prefix}${formatter.format(finalValue)}`;
    optionABtn.innerText = `Option A: decode 7 sequences for ${prefix}${formatter.format(finalValue)}`;
    if (counter >= 5) {
      confirmationMessage.innerText = `Based on your choices, you are willing to decode 7 sequences for ${prefix}${formatter.format(finalValue)} or more, but not for less. If you confirm the choice, check the checkbox; otherwise click the "Reset" button and make your decisions again.`;
      confirmChoice.hidden = false;
    }
  }

  function reset() {
    console.log("Resetting...");
    counter = 0;
    value = 200;
    min = 0;
    max = 400;
    confirmChoice.hidden = true;
    return false;
  }

  function makeButton(label: string, cb: (event: Event) => any) {
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
