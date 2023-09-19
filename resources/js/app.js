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
})();
