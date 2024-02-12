/* global Sentry, up*/
(function() {
  if (window.Sentry !== undefined) {
    Sentry.onLoad(function() {
      var pidEl = document.querySelector("[data-participant-id]");
      if (pidEl) {
        Sentry.setUser({
          id: pidEl.dataset.participantId
        });
      }
    });
  }

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

  up.compiler("[data-mask]", function(el) {
    let form = el.closest("form");
    let groups = form.querySelectorAll("[data-mask-group]");

    el.addEventListener("change", function() {
      reset();
    });

    reset();
    return;

    function reset() {
      let active = el.value;
      groups.forEach(function(g) {
        if (g.dataset.maskGroup === active) {
          g.style.display = "block";
        } else {
          g.style.display = "none";
        }
      });
    }
  });

  up.compiler("[data-counter]", function(el) {
    let prefix = el.dataset.counterPrefix || "";
    let min = el.min && el.min * 100;
    let max = el.max && el.max * 100;
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
      minusBtn.parent.removeChild(minusBtn);
      plusBtn.parent.removeChild(plusBtn);
    };

    function update() {
      let formatter = Intl.NumberFormat(undefined, {
        minimumFractionDigits: 2
      });
      el.value = `${value / 100}`;
      label.innerText = `${prefix}${formatter.format(el.value)}`;
    }

    function makeButton(label, cb) {
      let elt = document.createElement("button");
      elt.type = "button";
      elt.innerText = label;
      elt.addEventListener("click", e => {
        cb(e);
        update();
        return false;
      });
      return elt;
    }
  });

  up.compiler("[data-calibration]", function(el) {
    let prefix = el.dataset.calibrationPrefix || "£";
    let min = el.min && el.min * 100;
    let max = el.max && el.max * 100;
    let step = Number(el.step) * 100;
    let value = 200;
    let counter = 0;
    let group = el.closest(".group"); // FIXME: Needs to change for Lioness
    let formatter = Intl.NumberFormat(undefined, {
      minimumFractionDigits: 2
    });
    let confirmChoice = document.getElementById("confirm-choice");
    confirmChoice.hidden = true;
    let confirmationMessage = document.getElementById("confirmation-message");
    let resetBtn = document.getElementById("reset-button");
    resetBtn.addEventListener("click", e => {
      reset();
      return false;
    });

    let optionALabel = `Option A: decode 7 sequences for ${prefix}${formatter.format(value / 100)}`
    let optionABtn = makeButton(optionALabel, () => {
      if ( counter < 5 ) {
      max = value;
        value = (value + min)/2;
        value = Math.round(value / 10) * 10
        counter += 1;
      }
    });

    let optionBLabel = `Option B: decode 0 sequences for ${prefix}0.00`
    let optionBBtn = makeButton(optionBLabel, () => {
      if ( counter < 5 ) {
        min = value;
        value = (value + max)/2;
        value = Math.round(value / 10) * 10
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
      optionABtn.parent.removeChild(optionABtn);
      optionBBtn.parent.removeChild(optionBBtn);
    };

    function update() {
      el.value = `${value / 100}`;
      label.innerText = `${prefix}${formatter.format(el.value)}`;
      optionABtn.innerText = `Option A: decode 7 sequences for ${prefix}${formatter.format(value / 100)}`;
      if ( counter >= 5 ) {
        confirmationMessage.innerText = `Based on your choices, you are willing to decode 7 sequences for ${prefix}${formatter.format(value / 100)} or more, but not for less. If you confirm the choice, check the checkbox; otherwise click the "Reset" button and make your decisions again.`;
        confirmChoice.hidden = false;
      }
    }

    function reset() {
      console.log("Resetting...")
      counter = 0;
      value = 200;
      min = 0;
      max = 400;
      confirmChoice.hidden = true;
      return false;
    }

    function makeButton(label, cb) {
      let elt = document.createElement("button");
      elt.type = "button";
      elt.innerText = label;
      elt.addEventListener("click", e => {
        cb(e);
        update();
        return false;
      });
      return elt;
    }
  });

  up.compiler("[data-track-timings]", function(el) {
    const params = new URL(window.location.href).searchParams;
    const totalStart = time() - params.get("__tt") * 1;
    let focusStart = time() - params.get("__ft") * 1;
    let focusTotal = 0;

    el.querySelectorAll("a.button").forEach(el => {
      el.addEventListener("click", onButtonClick);
    });
    el.querySelectorAll("form").forEach(el => {
      el.addEventListener("submit", onFormSubmit);
    });
    document.addEventListener("visibilitychange", onVisibilityChange);
    return function() {
      el.querySelectorAll("a.button").forEach(el => {
        el.removeEventListener("click", onButtonClick);
      });
      el.querySelectorAll("form").forEach(el => {
        el.removeEventListener("submit", onFormSubmit);
      });
      document.removeEventListener("visibilitychange", onVisibilityChange);
    };

    function time() {
      return new Date().getTime();
    }

    function enrich(urlString) {
      const tt = (time() - totalStart).toString();
      const ft = (time() - focusStart + focusTotal).toString();
      const url = new URL(urlString);
      url.searchParams.set("__tt", tt);
      url.searchParams.set("__ft", ft);
      return url.toString();
    }

    function onButtonClick(e) {
      e.target.href = enrich(e.target.href);
      return true;
    }

    function onFormSubmit(e) {
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
})();
